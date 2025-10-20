BOT_NAME = "scrapy_tp"

SPIDER_MODULES = ["scrapy_tp.spiders"]
NEWSPIDER_MODULE = "scrapy_tp.spiders"

ROBOTSTXT_OBEY = False
DOWNLOAD_DELAY = 0.5
CONCURRENT_REQUESTS = 8
CONCURRENT_REQUESTS_PER_DOMAIN = 4
AUTOTHROTTLE_ENABLED = True
AUTOTHROTTLE_START_DELAY = 0.75
AUTOTHROTTLE_MAX_DELAY = 5.0
LOG_LEVEL = "INFO"
FEED_EXPORT_ENCODING = "utf-8"

DEFAULT_REQUEST_HEADERS = {
    "User-Agent": ("Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
                   "AppleWebKit/537.36 (KHTML, like Gecko) "
                   "Chrome/120.0.0.0 Safari/537.36")
}

FEEDS = {
    "outputs/f1_data.json": {
        "format": "json",
        "encoding": "utf-8",
        "overwrite": True,
        "indent": 2,
    }
}

ITEM_PIPELINES = {
    "scrapy_tp.pipelines.DropEmptyFieldsPipeline": 200,
    "scrapy_tp.pipelines.NormalizeStringsPipeline": 210,
    "scrapy_tp.pipelines.ChartPipeline": 900,
}

