from __future__ import annotations
import os
import re
import math
from typing import Any, Dict, List
from scrapy import signals

class DropEmptyFieldsPipeline:
    def process_item(self, item: Dict[str, Any], spider):
        def _clean_obj(x):
            if isinstance(x, dict):
                return {k: _clean_obj(v) for k, v in x.items() if v not in (None, "", [], {})}
            if isinstance(x, list):
                return [_clean_obj(v) for v in x if v not in (None, "", [], {})]
            return x
        return _clean_obj(item)

class NormalizeStringsPipeline:
    def process_item(self, item: Dict[str, Any], spider):
        def _norm(x):
            if isinstance(x, str):
                return re.sub(r"\s+", " ", x).strip()
            if isinstance(x, dict):
                return {k: _norm(v) for k, v in x.items()}
            if isinstance(x, list):
                return [_norm(v) for v in x]
            return x
        return _norm(item)

class ChartPipeline:
    """
    Un único gráfico de barras con:
      - escala logarítmica en Y
      - valores reales centrados en cada barra
      - leyenda 
    """
    def __init__(self):
        self.items: List[Dict[str, Any]] = []

    @classmethod
    def from_crawler(cls, crawler):
        obj = cls()
        crawler.signals.connect(obj.spider_closed, signal=signals.spider_closed)
        return obj

    def process_item(self, item: Dict[str, Any], spider):
        self.items.append(dict(item))
        return item

    # ---------- helpers ----------
    def _ensure_matplotlib(self, spider):
        try:
            import matplotlib
            matplotlib.use("Agg")  # backend sin GUI
            import matplotlib.pyplot as plt
            return matplotlib, plt
        except Exception as e:
            spider.logger.warning(f"No se pudo importar matplotlib: {e}")
            return None, None

    def _mk_outdir(self) -> str:
        outdir = os.path.join(os.getcwd(), "outputs")
        os.makedirs(outdir, exist_ok=True)
        return outdir

    def _num(self, x):
        return float(x) if isinstance(x, (int, float)) else None

    def _fmt_val(self, v: float) -> str:
        if v is None: return ""
        return f"{int(v)}" if float(v).is_integer() else f"{v:.1f}"

    def _fmt_tick(self, v: float) -> str:
        if v >= 1000:
            s = f"{int(v):,}".replace(",", "\u2009")
            return s
        return f"{int(v)}"

    def _nice_log_ticks(self, vmin: float, vmax: float) -> List[float]:
        if vmin <= 0: vmin = 1
        if vmax <= 0: vmax = 10
        pmin = int(math.floor(math.log10(vmin)))
        pmax = int(math.ceil(math.log10(vmax)))
        ticks = []
        for p in range(pmin, pmax + 1):
            for m in (1, 2, 5):
                t = m * (10 ** p)
                if vmin/1.001 <= t <= vmax*1.001:
                    ticks.append(t)
        if ticks and ticks[0] > vmin:
            ticks = [max(1, int(10 ** pmin))] + ticks
        return sorted(set(ticks))

    # ---------- gráfico ----------
    def spider_closed(self, spider):
        matplotlib, plt = self._ensure_matplotlib(spider)
        if not plt:
            return
        outdir = self._mk_outdir()

        # recolectar métricas por piloto
        names, season_pts, career_pts, wins, podiums, poles = [], [], [], [], [], []
        for it in self.items:
            if it.get("type") != "driver":
                continue
            nm = it.get("name")
            st = it.get("season_stats") or {}
            ct = it.get("career_stats") or {}
            sp = self._num(st.get("season_points"))
            cp = self._num(ct.get("career_points"))
            wi = self._num(st.get("grand_prix_wins")) or self._num(ct.get("grand_prix_wins"))
            po = self._num(st.get("grand_prix_podiums")) or self._num(ct.get("podiums"))
            pl = self._num(st.get("grand_prix_poles")) or self._num(ct.get("pole_positions"))
            if nm and any(v is not None for v in (sp, cp, wi, po, pl)):
                names.append(nm)
                season_pts.append(sp or 0.0)
                career_pts.append(cp or 0.0)
                wins.append(wi or 0.0)
                podiums.append(po or 0.0)
                poles.append(pl or 0.0)

        if not names:
            spider.logger.info("No hay datos de pilotos para graficar.")
            return
        
        series = [
            ("Season points", season_pts),
            ("Career points", career_pts),
            ("GP wins", wins),
            ("Podiums", podiums),
            ("Poles", poles),
        ]

        all_vals = [v for _, vals in series for v in vals]
        pos_vals = [v for v in all_vals if v > 0]
        min_pos = min(pos_vals) if pos_vals else 1.0
        eps = max(min_pos / 5.0, 0.1)

        series_plot = []
        for label, vals in series:
            vals_plot = [(eps if v <= 0 else v) for v in vals]
            series_plot.append((label, vals_plot, vals))

  
        n = len(names)
        m = len(series_plot)
        bar_width = 0.12 if m >= 5 else 0.16
        x = list(range(n))
        offsets = [(i - (m - 1) / 2) * bar_width for i in range(m)]

        fig_w = min(max(8, 0.9 * n), 18)
        fig_h = 6
        fig, ax = plt.subplots(figsize=(fig_w, fig_h))

        ax.set_yscale("log")
        ymax = max(max(vals) for _, vals, _ in series_plot) or 10
        ymin = min(min(vals) for _, vals, _ in series_plot) or eps
        ticks = self._nice_log_ticks(ymin, ymax)
        if ticks:
            ax.set_yticks(ticks)
            ax.set_yticklabels([self._fmt_tick(t) for t in ticks])

        for i, (label, vals_plot, vals_raw) in enumerate(series_plot):
            xi = [xx + offsets[i] for xx in x]
            bars = ax.bar(xi, vals_plot, width=bar_width, label=label)
            for rect, raw in zip(bars, vals_raw):
                cx = rect.get_x() + rect.get_width() / 2
                cy = rect.get_y() + rect.get_height() / 2
                txt = "0" if (raw is not None and raw == 0) else self._fmt_val(raw)
                ax.text(cx, cy, txt, ha="center", va="center", fontsize=9)

        ax.set_xticks(x)
        ax.set_xticklabels(names, rotation=25, ha="right")
        ax.set_ylabel("Valores")
        ax.set_title("Comparativa por piloto")

        ax.grid(axis="y", linestyle="--", alpha=0.35)

        h1, l1 = ax.get_legend_handles_labels()
        leg = ax.legend(h1, l1, ncol=3, fontsize=9, loc="upper center",
                        bbox_to_anchor=(0.5, -0.14), frameon=False)
        plt.subplots_adjust(bottom=0.22)

        plt.tight_layout()
        outfile = os.path.join(outdir, "g_all_metrics_log.png")
        plt.savefig(outfile, dpi=150)
        plt.close(fig)
        spider.logger.info(f"Gráfico guardado: {outfile}")
