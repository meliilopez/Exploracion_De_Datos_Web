import json
import re
import scrapy
from urllib.parse import urljoin


class FormulaOneSpider(scrapy.Spider):
    name = "formula_one"
    allowed_domains = ["www.formula1.com", "formula1.com"]

    # Solo en inglés para evitar 404
    start_urls = [
        "https://www.formula1.com/en/teams/ferrari",
        "https://www.formula1.com/en/drivers/lewis-hamilton",
    ]

    custom_settings = {
        "ROBOTSTXT_OBEY": False,
        "AUTOTHROTTLE_ENABLED": True,
        "AUTOTHROTTLE_START_DELAY": 0.75,
        "AUTOTHROTTLE_MAX_DELAY": 5.0,
        "CONCURRENT_REQUESTS": 8,
        "CONCURRENT_REQUESTS_PER_DOMAIN": 4,
        "DOWNLOAD_DELAY": 0.5,
        "HTTPCACHE_ENABLED": False,
        "FEED_EXPORT_ENCODING": "utf-8",
        "LOG_LEVEL": "INFO",
    }

    # ====== Mapeos ======
    _STAT_LABELS = {
        # Career
        "grand prix entered": ("career", "grands_prix_entered"),
        "career points": ("career", "career_points"),
        "highest race finish": ("career", "highest_race_finish"),
        "highest grid position": ("career", "highest_grid_position"),
        "podiums": ("career", "podiums"),
        "pole positions": ("career", "pole_positions"),
        "world championships": ("career", "world_championships"),
        "dnfs": ("career", "dnfs"),
        "fastest laps": ("career", "fastest_laps"),
        # Season
        "season position": ("season", "season_position"),
        "season points": ("season", "season_points"),
        "grand prix races": ("season", "grand_prix_races"),
        "grand prix points": ("season", "grand_prix_points"),
        "grand prix wins": ("season", "grand_prix_wins"),
        "grand prix podiums": ("season", "grand_prix_podiums"),
        "grand prix poles": ("season", "grand_prix_poles"),
        "dhl fastest laps": ("season", "dhl_fastest_laps"),
        "sprint races": ("season", "sprint_races"),
        "sprint points": ("season", "sprint_points"),
        "sprint wins": ("season", "sprint_wins"),
        "sprint podiums": ("season", "sprint_podiums"),
        "sprint poles": ("season", "sprint_poles"),
    }

    # ------------ helpers ------------
    def _clean(self, s):
        if s is None:
            return None
        return re.sub(r"\s+", " ", s).strip()

    def _iso_date(self, s):
        if not s:
            return None
        s = s.strip()
        if re.fullmatch(r"\d{4}-\d{2}-\d{2}", s):
            return s
        m = re.fullmatch(r"(\d{1,2})[/-](\d{1,2})[/-](\d{2,4})", s)
        if m:
            d, mth, y = m.groups()
            if len(y) == 2:
                y = ("19" if int(y) > 30 else "20") + y
            try:
                return f"{int(y):04d}-{int(mth):02d}-{int(d):02d}"
            except Exception:
                return None
        return s

    def _jsonld_objs(self, response):
        out = []
        for txt in response.xpath('//script[@type="application/ld+json"]/text()').getall():
            try:
                data = json.loads(txt)
                if isinstance(data, list):
                    out.extend(data)
                elif isinstance(data, dict):
                    out.append(data)
            except Exception:
                pass
        return out

    def _next_data(self, response):
        txt = response.xpath('//script[@id="__NEXT_DATA__"]/text()').get()
        if not txt:
            for c in response.xpath('//script[@type="application/json"]/text()').getall():
                if '"pageProps"' in c and '"props"' in c:
                    txt = c
                    break
        if not txt:
            return None
        try:
            return json.loads(txt)
        except Exception:
            return None

    def _deep_find_first(self, obj, wanted_keys):
        """Busca recursivamente primera coincidencia; si es dict, devuelve .name/.teamName/.displayName si existen."""
        if isinstance(obj, dict):
            for k, v in obj.items():
                if str(k).lower() in wanted_keys and v:
                    if isinstance(v, dict):
                        return v.get("name") or v.get("teamName") or v.get("displayName") or v
                    return v
                res = self._deep_find_first(v, wanted_keys)
                if res:
                    return res
        elif isinstance(obj, list):
            for it in obj:
                res = self._deep_find_first(it, wanted_keys)
                if res:
                    return res
        return None

    def _to_number(self, x):
        """
        Devuelve int o float según corresponda.
        - '5004.5' -> 5004.5
        """
        if x is None:
            return None
        if isinstance(x, (int, float)):
            return x
        s = str(x)
        m = re.search(r"-?\d+(?:[.,]\d+)?", s)
        if not m:
            return None
        val = m.group(0).replace(",", ".")
        return float(val) if "." in val else int(val)

    def _parse_best_with_count(self, text):
        if not text:
            return None, None
        t = str(text)
        m = re.search(r"[Pp]?\s*(\d+)\s*(?:\(\s*x\s*(\d+)\s*\))?", t)
        if not m:
            return None, None
        pos = int(m.group(1))
        cnt = int(m.group(2)) if m.lastindex and m.group(2) else None
        return pos, cnt

    def _normalize_driver_name(self, s: str) -> str:
        """Quita sufijos como 'Ferrari' y 'Flag of ...' del nombre del piloto."""
        if not s:
            return s
        s = re.sub(r"\bFlag of [A-Za-z ]+\b", "", s)
        s = re.sub(r"\s*-\s*F1 Driver\b.*$", "", s)
        s = s.replace("  ", " ").strip(" -")
        s = re.sub(r"\s+Ferrari$", "", s, flags=re.I)
        return self._clean(s)

    def _sanitize_loc(self, s: str):
        """Limpia y valida un lugar tipo 'Ciudad, País'; descarta textos largos/promos y rótulos."""
        s = self._clean(s)
        if not s:
            return None
        s = re.sub(r"^\s*(Place of birth|Lugar de nacimiento)\s*[:\-]?\s*", "", s, flags=re.I)
        if re.search(r"\b(Play|Related|Videos?)\b", s, flags=re.I):
            return None
        s = re.sub(r"[^A-Za-zÀ-ÿ' \-.,]", " ", s)
        s = re.sub(r"\s+", " ", s).strip(" ,.-")
        if len(s) > 60:
            s = s[:60].rstrip(" ,.-")
        if "," not in s and len(s.split()) < 2:
            return None
        return s

    def _strip_driver_name(self, s: str, driver_name: str):
        """Elimina el nombre del piloto si aparece dentro del string de ubicación."""
        if not s or not driver_name:
            return s
        base = re.sub(r"\s*-\s*F1 Driver\b.*$", "", driver_name).strip()
        parts = [p for p in re.split(r"\s+", base) if p and p.lower() not in {"-","–","—"}]
        if len(parts) >= 2:
            pat2 = r"\b" + re.escape(parts[-2]) + r"\s+" + re.escape(parts[-1]) + r"\b"
            s = re.sub(pat2, "", s, flags=re.I)
        pat1 = r"\b" + re.escape(parts[-1]) + r"\b"
        s = re.sub(pat1, "", s, flags=re.I)
        return self._clean(s)

    def _extract_place_of_birth(self, response, nxt, jsonlds):
        """Extrae place_of_birth preferentemente de JSON-LD/NEXT; si no, de selectores acotados."""
        for obj in (jsonlds or []):
            bp = obj.get("birthPlace") or obj.get("birthplace")
            if bp:
                val = bp.get("name") if isinstance(bp, dict) else bp
                val = self._sanitize_loc(val)
                if val:
                    return val

        if nxt:
            val = self._deep_find_first(nxt, {"birthplace", "birth_place", "birthPlace", "placeOfBirth"})
            val = self._sanitize_loc(val)
            if val:
                return val

        xpaths = [
            '//dl[.//dt[contains(., "Place of birth")]]/dd[1]//text()',
            '//*[contains(., "Place of birth")]/following::*[self::span or self::div][1]//text()',
            '//*[@data-test="place-of-birth"]//text()',
            '//span[contains(@class,"place-of-birth") or contains(@data-field,"birthPlace")]//text()',
        ]
        for xp in xpaths:
            txt = self._clean(" ".join(response.xpath(xp).getall()))
            val = self._sanitize_loc(txt)
            if val:
                return val

        full = " ".join(response.xpath("//main//text()[string-length(normalize-space())<80]").getall())
        m = re.search(r"\b([A-Za-zÀ-ÿ' \-]+,\s*[A-Za-zÀ-ÿ' \-]+)\b", full)
        if m:
            val = self._sanitize_loc(m.group(1))
            if val:
                return val

        return None

    # ------------ router ------------
    def parse(self, response):
        self.logger.info(f"INDEX -> {response.url}")

        if re.search(r"/en/drivers/lewis-hamilton/?$", response.url):
            # Llamamos directo sin Playwright ni request extra
            yield from self.parse_driver(response)
            return

        if re.search(r"/en/teams/ferrari/?$", response.url):
            yield from self.parse_team(response)
            return

    # ------------ driver ------------
    def parse_driver(self, response):
        url = response.url
        self.logger.info(f"DRIVER -> {url}")

        og_title = self._clean(response.css('meta[property="og:title"]::attr(content)').get())
        name = self._clean(response.css("h1::text").get()) or og_title

        nationality = None
        date_of_birth = None
        place_of_birth = None
        team = None

        #  JSON-LD (priorizar worksFor/memberOf para team)
        for obj in self._jsonld_objs(response):
            if obj.get("@type") in ("Person", "ProfilePage"):
                nationality = nationality or self._clean(obj.get("nationality"))
                date_of_birth = date_of_birth or self._clean(obj.get("birthDate"))
                bp = obj.get("birthPlace") or obj.get("birthplace")
                if bp and not place_of_birth:
                    place_of_birth = self._clean(bp.get("name") if isinstance(bp, dict) else bp)
                works_for = obj.get("worksFor") or obj.get("memberOf")
                if works_for and not team and isinstance(works_for, dict):
                    team = works_for.get("name") or works_for.get("teamName") or works_for.get("displayName")

        # __NEXT_DATA__
        nxt = self._next_data(response)
        if nxt:
            date_of_birth = date_of_birth or self._deep_find_first(
                nxt, {"birthdate", "birth_date", "birthDate", "dateOfBirth"}
            )
            nationality = nationality or self._deep_find_first(nxt, {"nationality"})
            if not team:
                tmp_team = self._deep_find_first(nxt, {"worksfor", "currentteam", "team"})
                if isinstance(tmp_team, str):
                    team = tmp_team
                elif isinstance(tmp_team, dict):
                    team = tmp_team.get("name") or tmp_team.get("teamName") or tmp_team.get("displayName")

        # Fallback por HTML cercano al encabezado
        if not team:
            href = response.xpath('(//main//a[starts-with(@href,"/en/teams/")]/@href)[1]').get()
            if href:
                slug = href.rstrip("/").split("/")[-1]
                team = slug.replace("-", " ").title()

        # Último recurso: inferir por título
        if not team and og_title and "ferrari" in og_title.lower():
            team = "Ferrari"

        # Normalizar básicos
        if isinstance(place_of_birth, dict):
            place_of_birth = place_of_birth.get("name")
        date_of_birth = self._iso_date(self._clean(date_of_birth)) if date_of_birth else None
        team = self._clean(team)

        # --------- estadísticas ----------
        season_stats, career_stats = self._extract_stats(response, nxt)

        # place_of_birth 
        if not place_of_birth:
            jsonlds = self._jsonld_objs(response)
            place_of_birth = self._extract_place_of_birth(response, nxt, jsonlds)

        place_of_birth = self._sanitize_loc(place_of_birth)
        place_of_birth = self._strip_driver_name(place_of_birth, name)

        # limpiar nombre por si vino con sufijos
        name = self._normalize_driver_name(name)

        # --- quitar campos problemáticos del output ---
        for k in ("grand_prix_top10s", "sprint_top10s"):
            if isinstance(season_stats, dict):
                season_stats.pop(k, None)

        yield {
            "type": "driver",
            "url": url,
            "name": name,
            "nationality": nationality,
            "team": team,
            "date_of_birth": date_of_birth,
            "place_of_birth": self._clean(place_of_birth),
            "season_stats": season_stats,
            "career_stats": career_stats,
        }

    # ------------ extracción de stats ------------
    def _extract_stats(self, response, nxt):
        season, career = {}, {}

        #  NEXT_DATA (por si vienen arrays {label,value})
        if nxt:
            blocks = [
                self._deep_find_first(nxt, {"statistics", "stats", "careerstats", "seasonstats"}),
                self._deep_find_first(nxt, {"driverstats"}),
                self._deep_find_first(nxt, {"bio", "about"}),
            ]
            for block in [b for b in blocks if isinstance(b, (dict, list))]:
                self._dig_stats_from_obj(block, season, career)

        sel = response.selector

        # HTML por secciones — localizar contenedores
        def section_container(selector, marker_regex):
            node = selector.xpath(
                f"//*[self::h1 or self::h2 or self::h3 or self::h4 or self::div]"
                f"[re:test(normalize-space(.), '{marker_regex}', 'i')][1]"
            )
            if node:
                container = node.xpath("./ancestor-or-self::div[1]") or node.xpath("./parent::div")
                return container or node
            return selector.xpath("/*[0]")

        season_box = section_container(sel, r"\bSEASON\b")
        career_box = section_container(sel, r"\bCAREER\s+STATS\b")

        if season_box:
            self._extract_section_cards(season_box, season, bucket="season")
        if career_box:
            self._extract_section_cards(career_box, career, bucket="career")

        return season, career

    def _extract_section_cards(self, box_sel, out_dict, bucket):
        for el in box_sel.css("*:not(script):not(style)"):
            txts = [self._clean(t) for t in el.css("::text").getall()]
            txt = " ".join([t for t in txts if t])
            if not txt or len(txt) > 120:
                continue
            low = txt.lower()

            # career
            if bucket == "career" and ("highest race finish" in low or "mejor resultado en carrera" in low):
                pos, cnt = self._parse_best_with_count(txt)
                if pos is not None:
                    out_dict["highest_race_finish"] = {"position": pos, **({"count": cnt} if cnt else {})}
                continue
            if bucket == "career" and ("highest grid position" in low or "mejor posición de largada" in low):
                pos, cnt = self._parse_best_with_count(txt)
                if pos is not None:
                    out_dict["highest_grid_position"] = {"position": pos, **({"count": cnt} if cnt else {})}
                continue

            # season
            if bucket == "season" and ("season position" in low or "posición de temporada" in low):
                m = re.search(r"[Pp]?(\d{1,2})", txt)
                if m:
                    out_dict["season_position"] = int(m.group(1))
                continue

            # Mapeo general
            for label, (bk, key) in self._STAT_LABELS.items():
                if bk != bucket:
                    continue
                if label in low:
                    num = self._to_number(txt)
                    if num is not None:
                        out_dict[key] = num if key == "career_points" else int(num)
                    break

    def _dig_stats_from_obj(self, obj, season, career):
        if isinstance(obj, dict):
            label = self._clean(obj.get("label")) if "label" in obj else None
            value = obj.get("value")
            if label and value is not None:
                low = label.lower()
                for k, (bucket, key) in self._STAT_LABELS.items():
                    if k in low:
                        if key in ("highest_race_finish", "highest_grid_position"):
                            pos, cnt = self._parse_best_with_count(value)
                            if pos is not None:
                                data = {"position": pos}
                                if cnt is not None:
                                    data["count"] = cnt
                                (season if bucket == "season" else career)[key] = data
                        elif key == "season_position":
                            num = self._to_number(value)
                            if num is not None:
                                (season if bucket == "season" else career)[key] = int(num)
                        else:
                            num = self._to_number(value)
                            if num is not None:
                                (season if bucket == "season" else career)[key] = num if key == "career_points" else int(num)
                        break
            for v in obj.values():
                self._dig_stats_from_obj(v, season, career)
        elif isinstance(obj, list):
            for it in obj:
                self._dig_stats_from_obj(it, season, career)

    # ------------ team (Ferrari) ------------
    def parse_team(self, response):
        url = response.url
        self.logger.info(f"TEAM -> {url}")

        team_name = (
            self._clean(response.css("h1::text").get())
            or self._clean(response.css('meta[property="og:title"]::attr(content)').get())
        )
        team_name_clean = (team_name or "").split(" - ")[0] 

        #  Intentar roster desde __NEXT_DATA__
        drivers = []
        nxt = self._next_data(response)
        if nxt:
            def _collect_names(obj):
                out = []
                if isinstance(obj, dict):
                    for k, v in obj.items():
                        kl = str(k).lower()
                        if kl in {"drivers", "currentdrivers", "teamdrivers", "driverlist"} and isinstance(v, list):
                            for d in v:
                                if isinstance(d, dict):
                                    nm = d.get("name") or " ".join(
                                        [d.get("firstName") or "", d.get("lastName") or ""]
                                    ).strip()
                                    if nm:
                                        out.append(nm)
                        out.extend(_collect_names(v))
                elif isinstance(obj, list):
                    for it in obj:
                        out.extend(_collect_names(it))
                return out

            drivers = _collect_names(nxt)
            drivers = [self._clean(x) for x in drivers if self._clean(x)]
            if len(drivers) > 3:
                drivers = list(dict.fromkeys(drivers))[:3]

        # Fallback por HTML: solo anchors con mención del team cerca
        if not drivers:
            cand = []
            for a in response.css('a[href^="/en/drivers/"]'):
                txt = self._clean(" ".join(a.css("::text").getall())) or ""
                near = " ".join(a.xpath(".//ancestor-or-self::*[self::a or self::div or self::li][1]//text()").getall())
                near = self._clean(near) or ""
                blob = (txt + " " + near).lower()
                if team_name_clean and team_name_clean.lower() in blob:
                    txt = re.sub(r"\bFlag of [A-Za-z ]+\b", "", txt).strip(" -")
                    m = re.match(r"([A-Za-zÀ-ÿ' \-]+)", txt)
                    nm = self._clean(m.group(1)) if m else self._clean(txt)
                    if nm:
                        cand.append(nm)
            drivers = list(dict.fromkeys([x for x in cand if x]))[:3]

        drivers = [self._normalize_driver_name(x) for x in drivers if x]

        yield {
            "type": "team",
            "url": url,
            "team_name": team_name_clean or team_name,
            "drivers": drivers,
            "details": {},
        }
