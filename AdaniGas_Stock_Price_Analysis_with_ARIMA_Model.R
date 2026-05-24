# ============================================================================
# ADANI GAS - COMPLETE FINANCIAL ANALYSIS PROJECT
# Company: Adani Total Gas Limited (ATGL.NS)
# ============================================================================
# This single file contains ALL parts:
# PART 01 : Financial Data Acquisition & Handling (CSV, Excel, API, Cleaning)
# PART 02 : Data Visualisation (ggplot2 - Line, Bar, Candlestick, OHLC)
# PART 03 : Basic Time Series Analysis (Trend, Decomposition, ARIMA)
# PART 04 : Algorithmic Trading (SMA Crossover, RSI, Performance Metrics)
# ============================================================================
cat("\n================================================================\n")
cat(" ADANI GAS - COMPLETE FINANCIAL ANALYSIS & TRADING PROJECT\n")
cat(" Company: Adani Total Gas Limited | Ticker: ATGL.NS\n")
cat("================================================================\n\n")
start_time <- Sys.time()
# ============================================================================
# INSTALL AND LOAD ALL REQUIRED PACKAGES
# ============================================================================
required_packages <- c(
  "quantmod", # Download financial data from Yahoo Finance API
  "tidyquant", # Tidy financial analysis
  "tidyverse", # Data manipulation & visualization (dplyr, ggplot2)
  "readxl", # Read Excel files
  "writexl", # Write Excel files
  "zoo", # Time series infrastructure
  "xts", # Extensible time series
  "lubridate", # Date manipulation
  "scales", # Axis formatting
  "forecast", # ARIMA modeling & forecasting
  "tseries", # Stationarity tests (ADF, KPSS)
  "TTR", # Technical trading indicators (SMA, RSI)
  "PerformanceAnalytics" # Portfolio performance metrics
)
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) {
  install.packages(new_packages, dependencies = TRUE, repos = "https://cran.r-project.org")
}
invisible(lapply(required_packages, library, character.only = TRUE))
cat("All packages loaded successfully.\n\n")
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PART 01: FINANCIAL DATA ACQUISITION & HANDLING
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cat("================================================================\n")
cat(" PART 01: FINANCIAL DATA ACQUISITION & HANDLING\n")
cat("================================================================\n\n")
# ---- 1. Define Parameters ---------------------------------------------------
ticker <- "ATGL.NS"
company <- "Adani Total Gas Limited"
start_dt <- as.Date("2019-01-01")
end_dt <- Sys.Date()
cat(paste("Downloading data for:", company, "(", ticker, ")\n"))
cat(paste("Period:", start_dt, "to", end_dt, "\n\n"))
# ---- 2. Download Data: Method 1 - quantmod (xts object) --------------------
atgl_xts <- getSymbols(
  Symbols = ticker,
  src = "yahoo",
  from = start_dt,
  to = end_dt,
  auto.assign = FALSE
)
cat(paste("Downloaded", nrow(atgl_xts), "rows via quantmod.\n"))
cat("Columns:", paste(names(atgl_xts), collapse = ", "), "\n\n")
# ---- 3. Download Data: Method 2 - tidyquant (tidy data frame) ---------------
atgl_tidy <- tq_get(
  x = ticker,
  get = "stock.prices",
  from = start_dt,
  to = end_dt
)
cat(paste("Downloaded", nrow(atgl_tidy), "rows via tidyquant.\n"))
cat("Columns:", paste(names(atgl_tidy), collapse = ", "), "\n\n")
cat("--- First 6 rows (tidy format) ---\n")
print(head(atgl_tidy))
cat("\n")
# ---- 4. Convert xts to Data Frame -------------------------------------------
atgl_df <- data.frame(
  Date = index(atgl_xts),
  Open = as.numeric(Op(atgl_xts)),
  High = as.numeric(Hi(atgl_xts)),
  Low = as.numeric(Lo(atgl_xts)),
  Close = as.numeric(Cl(atgl_xts)),
  Volume = as.numeric(Vo(atgl_xts)),
  Adjusted = as.numeric(Ad(atgl_xts)),
  stringsAsFactors = FALSE
)
cat("--- Structure of the data frame ---\n")
str(atgl_df)
cat("\n")
# ---- 5. Save Data to CSV ----------------------------------------------------
csv_path <- "Adani_Gas_Stock_Data.csv"
write.csv(atgl_df, file = csv_path, row.names = FALSE)
cat(paste("Data saved to CSV:", csv_path, "\n"))
# ---- 6. Save Data to Excel --------------------------------------------------
excel_path <- "Adani_Gas_Stock_Data.xlsx"
write_xlsx(atgl_df, path = excel_path)
cat(paste("Data saved to Excel:", excel_path, "\n\n"))
# ---- 7. Read Data Back from CSV ---------------------------------------------
atgl_csv <- read.csv(csv_path, stringsAsFactors = FALSE)
atgl_csv$Date <- as.Date(atgl_csv$Date)
cat("--- Data read back from CSV ---\n")
cat(paste("Rows:", nrow(atgl_csv), " | Columns:", ncol(atgl_csv), "\n"))
print(head(atgl_csv))
cat("\n")
# ---- 8. Read Data Back from Excel -------------------------------------------
atgl_excel <- read_excel(excel_path)
atgl_excel$Date <- as.Date(atgl_excel$Date)
cat("--- Data read back from Excel ---\n")
cat(paste("Rows:", nrow(atgl_excel), " | Columns:", ncol(atgl_excel), "\n"))
print(head(atgl_excel))
cat("\n")
# ---- 9. Data Cleaning -------------------------------------------------------
cat("=== DATA CLEANING ===\n\n")
# 9a. Check for missing values
cat("--- Missing Values Summary ---\n")
missing_summary <- colSums(is.na(atgl_df))
print(missing_summary)
cat(paste("\nTotal missing values:", sum(missing_summary), "\n\n"))
# 9b. Handle missing values using forward-fill (na.locf from zoo)
atgl_clean <- atgl_df
numeric_cols <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
for (col in numeric_cols) {
  if (any(is.na(atgl_clean[[col]]))) {
    atgl_clean[[col]] <- na.locf(atgl_clean[[col]], na.rm = FALSE)
    atgl_clean[[col]] <- na.locf(atgl_clean[[col]], fromLast = TRUE, na.rm = FALSE)
  }
}
cat("--- After cleaning: Missing Values ---\n")
print(colSums(is.na(atgl_clean)))
cat("\n")
# 9c. Check for duplicate dates
dup_dates <- atgl_clean$Date[duplicated(atgl_clean$Date)]
cat(paste("Duplicate dates found:", length(dup_dates), "\n"))
if (length(dup_dates) > 0) {
  cat("Removing duplicates (keeping first occurrence)...\n")
  atgl_clean <- atgl_clean[!duplicated(atgl_clean$Date), ]
}
# 9d. Sort by date (ascending)
atgl_clean <- atgl_clean[order(atgl_clean$Date), ]
rownames(atgl_clean) <- NULL
# 9e. Check for data anomalies
cat("\n--- Checking for data anomalies ---\n")
anomalies <- atgl_clean %>%
  filter(Open <= 0 | High <= 0 | Low <= 0 | Close <= 0 | Volume < 0)
cat(paste("Rows with non-positive prices or negative volume:", nrow(anomalies), "\n"))
logical_errors <- atgl_clean %>% filter(High < Low)
cat(paste("Rows with High < Low errors:", nrow(logical_errors), "\n"))
# 9f. Calculate additional columns
atgl_clean <- atgl_clean %>%
  mutate(
    Daily_Return = (Close / lag(Close) - 1) * 100,
    Log_Return = log(Close / lag(Close)) * 100,
    Price_Range = High - Low,
    Avg_Price = (Open + High + Low + Close) / 4,
    Year = year(Date),
    Month = as.character(month(Date, label = TRUE)),
    Day_of_Week = as.character(wday(Date, label = TRUE)),
    Quarter = quarter(Date)
  )
# 9g. Summary statistics
cat("\n--- Summary Statistics (Cleaned Data) ---\n")
print(summary(atgl_clean[, c("Open", "High", "Low", "Close", "Volume", "Daily_Return")]))
# ---- 10. Save Cleaned Data --------------------------------------------------
write.csv(atgl_clean, "Adani_Gas_Cleaned_Data.csv", row.names = FALSE)
write_xlsx(atgl_clean, "Adani_Gas_Cleaned_Data.xlsx")
cat("\nCleaned data saved to CSV and Excel.\n")
# ---- 11. Final Data Overview ------------------------------------------------
cat("\n=== FINAL DATA OVERVIEW ===\n")
cat(paste("Company :", company, "\n"))
cat(paste("Ticker :", ticker, "\n"))
cat(paste("Date Range :", min(atgl_clean$Date), "to", max(atgl_clean$Date), "\n"))
cat(paste("Total Records :", nrow(atgl_clean), "\n"))
cat(paste("Columns :", ncol(atgl_clean), "\n"))
cat(paste("Column Names :", paste(names(atgl_clean), collapse = ", "), "\n"))
cat("\n=== PART 01 COMPLETED ===\n\n")
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PART 02: DATA VISUALISATION
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cat("================================================================\n")
cat(" PART 02: DATA VISUALISATION (ggplot2)\n")
cat("================================================================\n\n")
# Use cleaned data already in memory (atgl_clean)
atgl <- atgl_clean
# Ensure Month is an ordered factor for plotting
month_levels <- c("Jan","Feb","Mar","Apr","May","Jun",
                  "Jul","Aug","Sep","Oct","Nov","Dec")
atgl$Month <- factor(atgl$Month, levels = month_levels, ordered = TRUE)
# Set a professional ggplot theme
theme_finance <- theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5,
                              color = "#1a1a2e"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#555555"),
    plot.caption = element_text(size = 9, color = "#888888"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "#333333"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
# ---------- CHART 1: Closing Price Line Chart --------------------------------
cat("--- Creating Chart 1: Closing Price Line Chart ---\n")
p1 <- ggplot(atgl, aes(x = Date, y = Close)) +
  geom_line(color = "#0066cc", linewidth = 0.6, alpha = 0.9) +
  geom_smooth(method = "loess", se = TRUE, color = "#e74c3c",
              fill = "#fce4ec", linewidth = 0.8, span = 0.2) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Adani Gas - Closing Price Over Time",
    subtitle = paste("Daily closing prices from", min(atgl$Date), "to", max(atgl$Date)),
    x = "Date", y = "Closing Price (INR)",
    caption = "Source: Yahoo Finance | Ticker: ATGL.NS"
  ) +
  theme_finance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p1)
ggsave("Chart1_Closing_Price_Line.png", p1, width = 12, height = 6, dpi = 300)
cat("Chart 1 saved.\n\n")
# ---------- CHART 2: Volume Over Time ----------------------------------------
cat("--- Creating Chart 2: Volume Line Chart ---\n")
p2 <- ggplot(atgl, aes(x = Date, y = Volume)) +
  geom_area(fill = "#3498db", alpha = 0.3) +
  geom_line(color = "#2980b9", linewidth = 0.3) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = function(x) paste0(round(x / 1e6, 1), "M")) +
  labs(
    title = "Adani Gas - Trading Volume Over Time",
    subtitle = "Daily trading volume",
    x = "Date", y = "Volume (Millions)",
    caption = "Source: Yahoo Finance | Ticker: ATGL.NS"
  ) +
  theme_finance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2)
ggsave("Chart2_Volume_Line.png", p2, width = 12, height = 6, dpi = 300)
cat("Chart 2 saved.\n\n")
# ---------- CHART 3: Average Monthly Closing Price Bar Plot -------------------
cat("--- Creating Chart 3: Monthly Average Closing Price Bar Plot ---\n")
monthly_avg <- atgl %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(Avg_Close = mean(Close, na.rm = TRUE), .groups = "drop")
p3 <- ggplot(monthly_avg, aes(x = YearMonth, y = Avg_Close)) +
  geom_col(fill = "#27ae60", alpha = 0.85, width = 25) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Adani Gas - Average Monthly Closing Price",
    subtitle = "Monthly average of daily closing prices",
    x = "Month", y = "Average Closing Price (INR)",
    caption = "Source: Yahoo Finance | Ticker: ATGL.NS"
  ) +
  theme_finance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p3)
ggsave("Chart3_Monthly_Avg_Close_Bar.png", p3, width = 12, height = 6, dpi = 300)
cat("Chart 3 saved.\n\n")
# ---------- CHART 4: Yearly Average Returns Bar Plot --------------------------
cat("--- Creating Chart 4: Yearly Average Returns Bar Plot ---\n")
yearly_returns <- atgl %>%
  filter(!is.na(Daily_Return)) %>%
  group_by(Year) %>%
  summarise(
    Avg_Return = mean(Daily_Return, na.rm = TRUE),
    Total_Return = sum(Daily_Return, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Positive = Avg_Return > 0)
p4 <- ggplot(yearly_returns, aes(x = factor(Year), y = Avg_Return, fill = Positive)) +
  geom_col(alpha = 0.85, width = 0.6, show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_text(aes(label = paste0(round(Avg_Return, 3), "%"),
                vjust = ifelse(Avg_Return > 0, -0.5, 1.5)),
            size = 3.5, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = "#27ae60", "FALSE" = "#e74c3c")) +
  labs(
    title = "Adani Gas - Average Daily Return by Year",
    subtitle = "Green = Positive | Red = Negative",
    x = "Year", y = "Average Daily Return (%)",
    caption = "Source: Yahoo Finance | Ticker: ATGL.NS"
  ) +
  theme_finance
print(p4)
ggsave("Chart4_Yearly_Returns_Bar.png", p4, width = 10, height = 6, dpi = 300)
cat("Chart 4 saved.\n\n")
# ---------- CHART 5: Candlestick Chart (Last 90 Days) ------------------------
cat("--- Creating Chart 5: Candlestick Chart (Last 90 Days) ---\n")
last_90 <- tail(atgl, 90) %>%
  mutate(Direction = ifelse(Close >= Open, "Up", "Down"))
p5 <- ggplot(last_90, aes(x = Date)) +
  geom_segment(aes(xend = Date, y = Low, yend = High),
               color = "grey30", linewidth = 0.4) +
  geom_rect(aes(xmin = Date - 0.4, xmax = Date + 0.4,
                ymin = pmin(Open, Close), ymax = pmax(Open, Close),
                fill = Direction),
            alpha = 0.9) +
  scale_fill_manual(values = c("Up" = "#27ae60", "Down" = "#e74c3c")) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Adani Gas - Candlestick Chart (Last 90 Trading Days)",
    subtitle = "Green = Bullish (Close > Open) | Red = Bearish (Close < Open)",
    x = "Date", y = "Price (INR)", fill = "Direction",
    caption = "Source: Yahoo Finance | Ticker: ATGL.NS"
  ) +
  theme_finance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p5)
ggsave("Chart5_Candlestick.png", p5, width = 14, height = 7, dpi = 300)
cat("Chart 5 saved.\n\n")
# ---------- CHART 6: Daily Returns Distribution (Histogram + Density) ---------
cat("--- Creating Chart 6: Daily Returns Distribution ---\n")
atgl_returns <- atgl %>% filter(!is.na(Daily_Return))
mean_return <- mean(atgl_returns$Daily_Return, na.rm = TRUE)
p6 <- ggplot(atgl_returns, aes(x = Daily_Return)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60,
                 fill = "#3498db", alpha = 0.6, color = "white") +
  geom_density(color = "#e74c3c", linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = mean_return, linetype = "solid",
             color = "#e67e22", linewidth = 0.8) +
  annotate("text", x = mean_return + 1, y = Inf, vjust = 2,
           label = paste("Mean:", round(mean_return, 3), "%"),
           color = "#e67e22", fontface = "bold", size = 4) +
  labs(
    title = "Adani Gas - Distribution of Daily Returns",
    subtitle = "Histogram with density curve overlay",
    x = "Daily Return (%)", y = "Density",
    caption = "Source: Yahoo Finance | Ticker: ATGL.NS"
  ) +
  theme_finance
print(p6)
ggsave("Chart6_Returns_Distribution.png", p6, width = 10, height = 6, dpi = 300)
cat("Chart 6 saved.\n\n")
# ---------- CHART 7: Moving Averages (50-day & 200-day SMA) ------------------
cat("--- Creating Chart 7: Moving Averages ---\n")
atgl_ma <- atgl %>%
  arrange(Date) %>%
  mutate(
    SMA_50 = rollmean(Close, k = 50, fill = NA, align = "right"),
    SMA_200 = rollmean(Close, k = 200, fill = NA, align = "right")
  )
p7 <- ggplot(atgl_ma, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Close Price"), linewidth = 0.5, alpha = 0.7) +
  geom_line(aes(y = SMA_50, color = "50-Day SMA"), linewidth = 0.8, na.rm = TRUE) +
  geom_line(aes(y = SMA_200, color = "200-Day SMA"), linewidth = 0.8, na.rm = TRUE) +
  scale_color_manual(values = c(
    "Close Price" = "#95a5a6",
    "50-Day SMA" = "#e74c3c",
    "200-Day SMA" = "#2980b9"
  )) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Adani Gas - Price with 50-Day & 200-Day Moving Averages",
    subtitle = "Simple Moving Averages (SMA) overlay on closing price",
    x = "Date", y = "Price (INR)", color = "Legend",
    caption = "Source: Yahoo Finance | Ticker: ATGL.NS"
  ) +
  theme_finance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p7)
ggsave("Chart7_Moving_Averages.png", p7, width = 12, height = 6, dpi = 300)
cat("Chart 7 saved.\n\n")
# ---------- CHART 8: Monthly Return Distribution Box Plot ---------------------
cat("--- Creating Chart 8: Monthly Return Distribution Box Plot ---\n")
p8 <- ggplot(atgl %>% filter(!is.na(Daily_Return)),
             aes(x = Month, y = Daily_Return, fill = Month)) +
  geom_boxplot(alpha = 0.7, outlier.color = "#e74c3c",
               outlier.shape = 16, outlier.size = 1.5,
               show.legend = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  scale_fill_viridis_d(option = "C") +
  labs(
    title = "Adani Gas - Daily Return Distribution by Month",
    subtitle = "Box plot showing spread, median, and outliers",
    x = "Month", y = "Daily Return (%)",
    caption = "Source: Yahoo Finance | Ticker: ATGL.NS"
  ) +
  theme_finance
print(p8)
ggsave("Chart8_Monthly_Boxplot.png", p8, width = 12, height = 6, dpi = 300)
cat("Chart 8 saved.\n\n")
# ---------- CHART 9: OHLC Bar Chart (Last 60 Days) ---------------------------
cat("--- Creating Chart 9: OHLC Bar Chart (Last 60 Days) ---\n")
last_60 <- tail(atgl, 60) %>%
  mutate(Direction = ifelse(Close >= Open, "Up", "Down"))
p9 <- ggplot(last_60, aes(x = Date)) +
  geom_segment(aes(xend = Date, y = Low, yend = High, color = Direction),
               linewidth = 0.6) +
  geom_segment(aes(x = Date - 0.4, xend = Date, y = Open, yend = Open,
                   color = Direction), linewidth = 0.6) +
  geom_segment(aes(x = Date, xend = Date + 0.4, y = Close, yend = Close,
                   color = Direction), linewidth = 0.6) +
  scale_color_manual(values = c("Up" = "#27ae60", "Down" = "#e74c3c")) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Adani Gas - OHLC Bar Chart (Last 60 Trading Days)",
    subtitle = "Open-High-Low-Close bars | Green = Bullish | Red = Bearish",
    x = "Date", y = "Price (INR)", color = "Direction",
    caption = "Source: Yahoo Finance | Ticker: ATGL.NS"
  ) +
  theme_finance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p9)
ggsave("Chart9_OHLC_Bar.png", p9, width = 14, height = 7, dpi = 300)
cat("Chart 9 saved.\n\n")
# ---------- CHART 10: Price vs Volume Dual Axis -------------------------------
cat("--- Creating Chart 10: Price vs Volume Combined ---\n")
vol_scale <- max(atgl$Close, na.rm = TRUE) / max(atgl$Volume, na.rm = TRUE)
p10 <- ggplot(atgl, aes(x = Date)) +
  geom_col(aes(y = Volume * vol_scale), fill = "#bdc3c7", alpha = 0.5) +
  geom_line(aes(y = Close), color = "#2c3e50", linewidth = 0.6) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(
    name = "Closing Price (INR)",
    labels = scales::comma,
    sec.axis = sec_axis(~ . / vol_scale,
                        name = "Volume",
                        labels = function(x) paste0(round(x / 1e6, 1), "M"))
  ) +
  labs(
    title = "Adani Gas - Price and Volume",
    subtitle = "Closing price (line) overlaid on volume (bars)",
    x = "Date",
    caption = "Source: Yahoo Finance | Ticker: ATGL.NS"
  ) +
  theme_finance +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p10)
ggsave("Chart10_Price_Volume.png", p10, width = 12, height = 6, dpi = 300)
cat("Chart 10 saved.\n\n")
cat("=== PART 02: DATA VISUALISATION COMPLETED (10 Charts) ===\n\n")
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PART 03: BASIC TIME SERIES ANALYSIS
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cat("================================================================\n")
cat(" PART 03: BASIC TIME SERIES ANALYSIS\n")
cat("================================================================\n\n")
# ---- 1. Create Time Series Objects ------------------------------------------
cat("=== CREATING TIME SERIES OBJECTS ===\n\n")
monthly_data <- atgl %>%
  mutate(YearMonth = floor_date(Date, "month")) %>%
  group_by(YearMonth) %>%
  summarise(
    Avg_Close = mean(Close, na.rm = TRUE),
    Avg_Volume = mean(Volume, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(YearMonth)
start_year <- year(min(monthly_data$YearMonth))
start_month <- month(min(monthly_data$YearMonth))
ts_close <- ts(monthly_data$Avg_Close,
               start = c(start_year, start_month),
               frequency = 12)
cat("Time series object created:\n")
cat(paste(" Start:", paste(start(ts_close), collapse = "."), "\n"))
cat(paste(" End :", paste(end(ts_close), collapse = "."), "\n"))
cat(paste(" Freq :", frequency(ts_close), "(monthly)\n"))
cat(paste(" Obs :", length(ts_close), "\n\n"))
ts_daily <- xts(atgl$Close, order.by = atgl$Date)
# ---- 2. TREND ANALYSIS ------------------------------------------------------
cat("=== TREND ANALYSIS ===\n\n")
p_ts <- ggplot(monthly_data, aes(x = YearMonth, y = Avg_Close)) +
  geom_line(color = "#2c3e50", linewidth = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "#e74c3c",
              fill = "#fce4ec", linewidth = 1, span = 0.3) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Adani Gas - Monthly Average Closing Price (Trend)",
    subtitle = "LOESS smoothing curve shows the underlying trend",
    x = "Date", y = "Average Closing Price (INR)",
    caption = "Source: Yahoo Finance | ATGL.NS"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
print(p_ts)
ggsave("TS_Trend_Analysis.png", p_ts, width = 12, height = 6, dpi = 300)
# Linear Trend Model
time_index <- 1:length(ts_close)
trend_model <- lm(as.numeric(ts_close) ~ time_index)
cat("--- Linear Trend Model ---\n")
cat(paste(" Intercept :", round(coef(trend_model)[1], 4), "\n"))
cat(paste(" Slope :", round(coef(trend_model)[2], 4), "\n"))
cat(paste(" R-squared :", round(summary(trend_model)$r.squared, 4), "\n"))
cat(paste(" p-value :", format(summary(trend_model)$coefficients[2, 4],
                               scientific = TRUE), "\n\n"))
if (coef(trend_model)[2] > 0) {
  cat(" Interpretation: The stock shows an UPWARD trend over time.\n\n")
} else {
  cat(" Interpretation: The stock shows a DOWNWARD trend over time.\n\n")
}
# ---- 3. SEASONAL DECOMPOSITION ----------------------------------------------
cat("=== SEASONAL DECOMPOSITION ===\n\n")
if (length(ts_close) >= 24) {
  # Multiplicative Decomposition
  decomp_mult <- decompose(ts_close, type = "multiplicative")
  cat("--- Multiplicative Decomposition Components ---\n")
  cat(" Components: Observed, Trend, Seasonal, Random\n\n")
  png("TS_Decomposition_Multiplicative.png", width = 1200, height = 800, res = 150)
  plot(decomp_mult, col = "#2c3e50")
  title(main = "Adani Gas - Multiplicative Decomposition", outer = FALSE)
  dev.off()
  cat("Multiplicative decomposition plot saved.\n")
  # Additive Decomposition
  decomp_add <- decompose(ts_close, type = "additive")
  png("TS_Decomposition_Additive.png", width = 1200, height = 800, res = 150)
  plot(decomp_add, col = "#2c3e50")
  title(main = "Adani Gas - Additive Decomposition", outer = FALSE)
  dev.off()
  cat("Additive decomposition plot saved.\n")
  # STL Decomposition
  stl_decomp <- stl(ts_close, s.window = "periodic")
  png("TS_STL_Decomposition.png", width = 1200, height = 800, res = 150)
  plot(stl_decomp, col = "#2c3e50")
  title(main = "Adani Gas - STL Decomposition", outer = FALSE)
  dev.off()
  cat("STL decomposition plot saved.\n\n")
  # Seasonal factors
  cat("--- Seasonal Factors (Multiplicative) ---\n")
  seasonal_factors <- decomp_mult$figure
  names(seasonal_factors) <- month.abb
  print(round(seasonal_factors, 4))
  cat("\n")
} else {
  cat("Not enough data for seasonal decomposition (need >= 24 months).\n\n")
}
# ---- 4. STATIONARITY TESTS --------------------------------------------------
cat("=== STATIONARITY TESTS ===\n\n")
# ADF Test on raw prices
cat("--- ADF Test on Raw Closing Prices ---\n")
adf_raw <- adf.test(ts_close, alternative = "stationary")
cat(paste(" Test Statistic :", round(adf_raw$statistic, 4), "\n"))
cat(paste(" p-value :", round(adf_raw$p.value, 4), "\n"))
if (adf_raw$p.value < 0.05) {
  cat(" Result: Series IS stationary (reject H0) at 5% significance.\n\n")
} else {
  cat(" Result: Series is NOT stationary (fail to reject H0) at 5% significance.\n\n")
}
# ADF Test on first differences
ts_diff <- diff(ts_close)
cat("--- ADF Test on First Differences (Returns) ---\n")
adf_diff <- adf.test(ts_diff, alternative = "stationary")
cat(paste(" Test Statistic :", round(adf_diff$statistic, 4), "\n"))
cat(paste(" p-value :", round(adf_diff$p.value, 4), "\n"))
if (adf_diff$p.value < 0.05) {
  cat(" Result: Differenced series IS stationary (reject H0).\n\n")
} else {
  cat(" Result: Differenced series is NOT stationary.\n\n")
}
# KPSS Test
cat("--- KPSS Test on Raw Prices ---\n")
kpss_raw <- kpss.test(ts_close, null = "Level")
cat(paste(" Test Statistic :", round(kpss_raw$statistic, 4), "\n"))
cat(paste(" p-value :", round(kpss_raw$p.value, 4), "\n"))
if (kpss_raw$p.value < 0.05) {
  cat(" Result: Series is NOT stationary (reject H0 of stationarity).\n\n")
} else {
  cat(" Result: Series IS stationary (fail to reject H0).\n\n")
}
ndiffs_val <- ndiffs(ts_close, test = "adf")
cat(paste("Number of differences needed (ADF):", ndiffs_val, "\n\n"))
# ---- 5. ACF and PACF Plots --------------------------------------------------
cat("=== ACF AND PACF ANALYSIS ===\n\n")
png("TS_ACF_Plot.png", width = 1000, height = 500, res = 150)
acf(ts_close, lag.max = 36, main = "Adani Gas - ACF of Monthly Closing Prices",
    col = "#2c3e50")
dev.off()
cat("ACF plot saved.\n")
png("TS_PACF_Plot.png", width = 1000, height = 500, res = 150)
pacf(ts_close, lag.max = 36, main = "Adani Gas - PACF of Monthly Closing Prices",
     col = "#e74c3c")
dev.off()
cat("PACF plot saved.\n")
png("TS_ACF_Diff.png", width = 1000, height = 500, res = 150)
acf(ts_diff, lag.max = 36,
    main = "Adani Gas - ACF of Differenced Series", col = "#2c3e50")
dev.off()
png("TS_PACF_Diff.png", width = 1000, height = 500, res = 150)
pacf(ts_diff, lag.max = 36,
     main = "Adani Gas - PACF of Differenced Series", col = "#e74c3c")
dev.off()
cat("ACF and PACF of differenced series saved.\n\n")
# ---- 6. ARIMA MODELING -------------------------------------------------------
cat("=== ARIMA MODELING ===\n\n")
# Auto ARIMA
cat("--- Fitting Auto ARIMA Model ---\n")
auto_model <- auto.arima(
  ts_close,
  seasonal = TRUE,
  stepwise = FALSE,
  approximation = FALSE,
  trace = TRUE
)
cat("\n--- Auto ARIMA Model Summary ---\n")
print(summary(auto_model))
arima_order <- arimaorder(auto_model)
cat(paste0("\n Selected Order: ARIMA(",
           arima_order[1], ",", arima_order[2], ",", arima_order[3], ")\n"))
cat(paste(" AIC :", round(AIC(auto_model), 2), "\n"))
cat(paste(" BIC :", round(BIC(auto_model), 2), "\n\n"))
# Manual ARIMA Models for comparison
cat("--- Fitting Manual ARIMA Models for Comparison ---\n\n")
models <- list()
model_specs <- list(
  c(1, 1, 0), c(0, 1, 1), c(1, 1, 1),
  c(2, 1, 1), c(1, 1, 2), c(2, 1, 2)
)
for (spec in model_specs) {
  model_name <- paste0("ARIMA(", spec[1], ",", spec[2], ",", spec[3], ")")
  tryCatch({
    models[[model_name]] <- Arima(ts_close, order = spec)
  }, error = function(e) {
    cat(paste(" Warning: Could not fit", model_name, "-", e$message, "\n"))
  })
}
if (length(models) > 0) {
  model_comparison <- data.frame(
    Model = names(models),
    AIC = sapply(models, AIC),
    BIC = sapply(models, BIC),
    stringsAsFactors = FALSE
  )
  model_comparison <- model_comparison[order(model_comparison$AIC), ]
  rownames(model_comparison) <- NULL
  cat("--- Model Comparison (sorted by AIC) ---\n")
  print(model_comparison, row.names = FALSE)
  cat(paste("\nBest model by AIC:", model_comparison$Model[1], "\n\n"))
}
# Residual Diagnostics
cat("--- Residual Diagnostics ---\n")
png("TS_ARIMA_Residuals.png", width = 1200, height = 800, res = 150)
checkresiduals(auto_model, plot = TRUE)
dev.off()
cat("Residual diagnostics plot saved.\n")
lb_test <- Box.test(residuals(auto_model), lag = 20, type = "Ljung-Box")
cat(paste(" Ljung-Box Test p-value:", round(lb_test$p.value, 4), "\n"))
if (lb_test$p.value > 0.05) {
  cat(" Result: No significant autocorrelation in residuals (good model fit).\n\n")
} else {
  cat(" Result: Significant autocorrelation in residuals (model may need improvement).\n\n")
}
shapiro_res <- shapiro.test(residuals(auto_model))
cat(paste(" Shapiro-Wilk Normality Test p-value:", round(shapiro_res$p.value, 4), "\n"))
if (shapiro_res$p.value > 0.05) {
  cat(" Result: Residuals are approximately normal.\n\n")
} else {
  cat(" Result: Residuals deviate from normality.\n\n")
}
# ---- 7. FORECASTING ---------------------------------------------------------
cat("=== FORECASTING ===\n\n")
forecast_horizon <- 12
fc <- forecast(auto_model, h = forecast_horizon)
cat(paste("--- Forecast for next", forecast_horizon, "months ---\n"))
print(fc)
cat("\n")
p_fc <- autoplot(fc) +
  labs(
    title = paste("Adani Gas - ARIMA Forecast (Next", forecast_horizon, "Months)"),
    subtitle = paste0("Model: ARIMA(",
                      arima_order[1], ",", arima_order[2], ",", arima_order[3], ")"),
    x = "Time", y = "Monthly Average Closing Price (INR)",
    caption = "Source: Yahoo Finance | ATGL.NS"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  )
print(p_fc)
ggsave("TS_ARIMA_Forecast.png", p_fc, width = 12, height = 6, dpi = 300)
cat("Forecast plot saved.\n\n")
# ---- 8. ACCURACY METRICS (Train-Test Split) ----------------------------------
cat("=== MODEL ACCURACY ===\n\n")
n_total <- length(ts_close)
n_train <- floor(0.8 * n_total)
n_test <- n_total - n_train
if (n_test >= 2) {
  ts_train <- ts(head(as.numeric(ts_close), n_train),
                 start = c(start_year, start_month), frequency = 12)
  ts_test <- ts(tail(as.numeric(ts_close), n_test),
                start = c(start_year + (n_train + start_month - 1) %/% 12,
                          (n_train + start_month - 1) %% 12 + 1),
                frequency = 12)
  cat(paste(" Train set:", n_train, "months\n"))
  cat(paste(" Test set :", n_test, "months\n\n"))
  train_model <- auto.arima(ts_train, seasonal = TRUE,
                            stepwise = FALSE, approximation = FALSE)
  fc_test <- forecast(train_model, h = n_test)
  acc <- accuracy(fc_test, ts_test)
  cat("--- Forecast Accuracy (Train vs Test) ---\n")
  print(round(acc, 4))
  cat("\n")
  p_acc <- autoplot(fc_test) +
    autolayer(ts_test, series = "Actual", linewidth = 1) +
    labs(
      title = "Adani Gas - ARIMA: Actual vs Forecast (Test Set)",
      subtitle = paste("Train:", n_train, "months | Test:", n_test, "months"),
      x = "Time", y = "Price (INR)",
      caption = "Source: Yahoo Finance | ATGL.NS"
    ) +
    scale_y_continuous(labels = scales::comma) +
    theme_minimal(base_size = 13) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  print(p_acc)
  ggsave("TS_Actual_vs_Forecast.png", p_acc, width = 12, height = 6, dpi = 300)
  cat("Actual vs Forecast plot saved.\n\n")
} else {
  cat("Not enough data for train-test split evaluation.\n\n")
}
cat("=== PART 03: TIME SERIES ANALYSIS COMPLETED ===\n\n")
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PART 04: ALGORITHMIC TRADING
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cat("================================================================\n")
cat(" PART 04: ALGORITHMIC TRADING\n")
cat("================================================================\n\n")
# ============================================================================
# STRATEGY 1: SIMPLE MOVING AVERAGE (SMA) CROSSOVER (50/200)
# ============================================================================
cat("=== STRATEGY 1: SMA CROSSOVER (50/200) ===\n\n")
short_window <- 50
long_window <- 200
atgl_strat <- atgl %>%
  arrange(Date) %>%
  mutate(
    SMA_Short = SMA(Close, n = short_window),
    SMA_Long = SMA(Close, n = long_window)
  ) %>%
  filter(!is.na(SMA_Long))
cat(paste("Short SMA window:", short_window, "days\n"))
cat(paste("Long SMA window :", long_window, "days\n"))
cat(paste("Trading days with signals:", nrow(atgl_strat), "\n\n"))
# Generate Trading Signals
atgl_strat <- atgl_strat %>%
  mutate(
    Signal = case_when(
      SMA_Short > SMA_Long ~ 1L,
      SMA_Short < SMA_Long ~ -1L,
      TRUE ~ 0L
    ),
    Position = lag(Signal, 1),
    Position = ifelse(is.na(Position), 0, Position),
    Crossover = Signal - lag(Signal, 1),
    Trade = case_when(
      Crossover == 2 ~ "BUY",
      Crossover == -2 ~ "SELL",
      TRUE ~ NA_character_
    )
  )
# List All Trade Signals
trades <- atgl_strat %>%
  filter(!is.na(Trade)) %>%
  select(Date, Close, SMA_Short, SMA_Long, Trade)
cat("--- Trade Signals ---\n")
print(as.data.frame(trades))
cat(paste("\nTotal BUY signals :", sum(trades$Trade == "BUY"), "\n"))
cat(paste("Total SELL signals:", sum(trades$Trade == "SELL"), "\n\n"))
# Calculate Strategy Returns
atgl_strat <- atgl_strat %>%
  mutate(
    Market_Return = Close / lag(Close) - 1,
    Strategy_Return = Position * Market_Return
  ) %>%
  filter(!is.na(Market_Return))
atgl_strat <- atgl_strat %>%
  mutate(
    Cum_Market = cumprod(1 + Market_Return),
    Cum_Strategy = cumprod(1 + Strategy_Return)
  )
cat("--- Cumulative Returns ---\n")
cat(paste("Buy & Hold Return :",
          round((tail(atgl_strat$Cum_Market, 1) - 1) * 100, 2), "%\n"))
cat(paste("Strategy Return :",
          round((tail(atgl_strat$Cum_Strategy, 1) - 1) * 100, 2), "%\n\n"))
# Plot: SMA Signals
cat("--- Creating Trading Strategy Plots ---\n\n")
sma_short_label <- paste0(short_window, "-Day SMA")
sma_long_label <- paste0(long_window, "-Day SMA")
color_vals <- c("#95a5a6", "#e67e22", "#2980b9")
names(color_vals) <- c("Close Price", sma_short_label, sma_long_label)
atgl_strat$SMA_Short_Label <- sma_short_label
atgl_strat$SMA_Long_Label <- sma_long_label
p_signals <- ggplot(atgl_strat, aes(x = Date)) +
  geom_line(aes(y = Close, color = "Close Price"), linewidth = 0.4, alpha = 0.7) +
  geom_line(aes(y = SMA_Short, color = SMA_Short_Label), linewidth = 0.7) +
  geom_line(aes(y = SMA_Long, color = SMA_Long_Label), linewidth = 0.7) +
  geom_point(data = atgl_strat %>% filter(Trade == "BUY"),
             aes(y = Close), color = "#27ae60", shape = 24,
             size = 3, fill = "#27ae60") +
  geom_point(data = atgl_strat %>% filter(Trade == "SELL"),
             aes(y = Close), color = "#e74c3c", shape = 25,
             size = 3, fill = "#e74c3c") +
  scale_color_manual(values = color_vals) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(
    title = "Adani Gas - SMA Crossover Trading Signals",
    subtitle = paste0(short_window, "/", long_window,
                      " SMA | BUY (Green) | SELL (Red)"),
    x = "Date", y = "Price (INR)", color = "Legend",
    caption = "Source: Yahoo Finance | Ticker: ATGL.NS"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
print(p_signals)
ggsave("Algo_SMA_Signals.png", p_signals, width = 14, height = 7, dpi = 300)
cat("SMA signals plot saved.\n")
# Plot: Cumulative Returns
p_cumret <- ggplot(atgl_strat, aes(x = Date)) +
  geom_line(aes(y = Cum_Market, color = "Buy and Hold"), linewidth = 0.8) +
  geom_line(aes(y = Cum_Strategy, color = "SMA Strategy"), linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c(
    "Buy and Hold" = "#3498db",
    "SMA Strategy" = "#e74c3c"
  )) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "x")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(
    title = "Adani Gas - Cumulative Returns: Strategy vs Buy and Hold",
    subtitle = paste0(short_window, "/", long_window, " SMA Crossover Strategy"),
    x = "Date", y = "Cumulative Return (multiple of initial investment)",
    color = "Strategy",
    caption = "Source: Yahoo Finance | Ticker: ATGL.NS"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
print(p_cumret)
ggsave("Algo_Cumulative_Returns.png", p_cumret, width = 12, height = 6, dpi = 300)
cat("Cumulative returns plot saved.\n\n")
# ============================================================================
# STRATEGY 2: RSI (Relative Strength Index) STRATEGY
# ============================================================================
cat("=== STRATEGY 2: RSI STRATEGY ===\n\n")
rsi_period <- 14
oversold_level <- 30
overbought_level <- 70
atgl_rsi <- atgl %>%
  arrange(Date) %>%
  mutate(RSI = RSI(Close, n = rsi_period)) %>%
  filter(!is.na(RSI))
atgl_rsi <- atgl_rsi %>%
  mutate(
    RSI_Signal = case_when(
      RSI < oversold_level ~ 1L,
      RSI > overbought_level ~ -1L,
      TRUE ~ 0L
    ),
    RSI_Position = NA_integer_
  )
# Forward-fill positions
current_pos <- 0L
for (i in seq_len(nrow(atgl_rsi))) {
  if (atgl_rsi$RSI_Signal[i] != 0) {
    current_pos <- atgl_rsi$RSI_Signal[i]
  }
  atgl_rsi$RSI_Position[i] <- current_pos
}
atgl_rsi <- atgl_rsi %>%
  mutate(
    RSI_Position = lag(RSI_Position, 1),
    RSI_Position = ifelse(is.na(RSI_Position), 0, RSI_Position),
    Market_Return = Close / lag(Close) - 1,
    RSI_Return = RSI_Position * Market_Return
  ) %>%
  mutate(
    Market_Return = ifelse(is.na(Market_Return), 0, Market_Return),
    RSI_Return = ifelse(is.na(RSI_Return), 0, RSI_Return),
    Cum_Market_RSI = cumprod(1 + Market_Return),
    Cum_RSI = cumprod(1 + RSI_Return)
  )
cat(paste("RSI Period :", rsi_period, "\n"))
cat(paste("Oversold (<) :", oversold_level, "\n"))
cat(paste("Overbought (>) :", overbought_level, "\n"))
cat(paste("Buy & Hold Return:",
          round((tail(atgl_rsi$Cum_Market_RSI, 1) - 1) * 100, 2), "%\n"))
cat(paste("RSI Strategy Ret :",
          round((tail(atgl_rsi$Cum_RSI, 1) - 1) * 100, 2), "%\n\n"))
# RSI Plot
p_rsi <- ggplot(atgl_rsi, aes(x = Date)) +
  geom_line(aes(y = RSI), color = "#8e44ad", linewidth = 0.5) +
  geom_hline(yintercept = oversold_level, linetype = "dashed",
             color = "#27ae60", linewidth = 0.6) +
  geom_hline(yintercept = overbought_level, linetype = "dashed",
             color = "#e74c3c", linewidth = 0.6) +
  geom_hline(yintercept = 50, linetype = "dotted", color = "grey50") +
  annotate("rect", xmin = min(atgl_rsi$Date), xmax = max(atgl_rsi$Date),
           ymin = 0, ymax = oversold_level,
           fill = "#27ae60", alpha = 0.08) +
  annotate("rect", xmin = min(atgl_rsi$Date), xmax = max(atgl_rsi$Date),
           ymin = overbought_level, ymax = 100,
           fill = "#e74c3c", alpha = 0.08) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(
    title = "Adani Gas - RSI (Relative Strength Index)",
    subtitle = paste0("Period: ", rsi_period,
                      " | Oversold < ", oversold_level,
                      " | Overbought > ", overbought_level),
    x = "Date", y = "RSI",
    caption = "Source: Yahoo Finance | Ticker: ATGL.NS"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(p_rsi)
ggsave("Algo_RSI.png", p_rsi, width = 12, height = 5, dpi = 300)
cat("RSI plot saved.\n")
# RSI Cumulative Returns
p_rsi_cum <- ggplot(atgl_rsi, aes(x = Date)) +
  geom_line(aes(y = Cum_Market_RSI, color = "Buy and Hold"), linewidth = 0.8) +
  geom_line(aes(y = Cum_RSI, color = "RSI Strategy"), linewidth = 0.8) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  scale_color_manual(values = c(
    "Buy and Hold" = "#3498db",
    "RSI Strategy" = "#8e44ad"
  )) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "x")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(
    title = "Adani Gas - Cumulative Returns: RSI Strategy vs Buy and Hold",
    subtitle = paste0("RSI(", rsi_period, ") | Buy when RSI < ", oversold_level,
                      " | Sell when RSI > ", overbought_level),
    x = "Date", y = "Cumulative Return", color = "Strategy",
    caption = "Source: Yahoo Finance | Ticker: ATGL.NS"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
print(p_rsi_cum)
ggsave("Algo_RSI_Cumulative.png", p_rsi_cum, width = 12, height = 6, dpi = 300)
cat("RSI cumulative returns plot saved.\n\n")
# ============================================================================
# PERFORMANCE METRICS
# ============================================================================
cat("=== COMPREHENSIVE PERFORMANCE METRICS ===\n\n")
sma_xts <- xts(
  data.frame(
    Market = atgl_strat$Market_Return,
    Strategy = atgl_strat$Strategy_Return
  ),
  order.by = atgl_strat$Date
)
sma_xts <- sma_xts[complete.cases(sma_xts), ]
cat("--- SMA CROSSOVER STRATEGY METRICS ---\n\n")
# Annualized Returns
annual_ret <- Return.annualized(sma_xts, scale = 252)
cat("Annualized Returns:\n")
print(round(annual_ret * 100, 2))
cat("\n")
# Annualized Volatility
annual_vol <- StdDev.annualized(sma_xts, scale = 252)
cat("Annualized Volatility:\n")
print(round(annual_vol * 100, 2))
cat("\n")
# Sharpe Ratio (6% risk-free rate for India)
rf_rate <- 0.06 / 252
sharpe <- SharpeRatio.annualized(sma_xts, Rf = rf_rate, scale = 252)
cat("Sharpe Ratio (Annualized, Rf=6%):\n")
print(round(sharpe, 4))
cat("\n")
# Maximum Drawdown
max_dd <- maxDrawdown(sma_xts)
cat("Maximum Drawdown:\n")
print(round(max_dd * 100, 2))
cat("\n")
# Sortino Ratio
sortino <- SortinoRatio(sma_xts, MAR = rf_rate)
cat("Sortino Ratio:\n")
print(round(sortino, 4))
cat("\n")
# Win Rate
strategy_returns <- atgl_strat$Strategy_Return[!is.na(atgl_strat$Strategy_Return)]
trading_days <- strategy_returns[strategy_returns != 0]
if (length(trading_days) > 0) {
  win_rate <- sum(trading_days > 0) / length(trading_days) * 100
} else {
  win_rate <- 0
}
cat(paste("Win Rate (trading days):", round(win_rate, 2), "%\n\n"))
# Calmar Ratio
calmar_market <- as.numeric(annual_ret[1, 1]) / as.numeric(max_dd[1])
calmar_strategy <- as.numeric(annual_ret[1, 2]) / as.numeric(max_dd[2])
cat(paste("Calmar Ratio (Market) :", round(calmar_market, 4), "\n"))
cat(paste("Calmar Ratio (Strategy):", round(calmar_strategy, 4), "\n\n"))
# Performance Summary Table
cat("=== PERFORMANCE SUMMARY TABLE ===\n\n")
perf_summary <- data.frame(
  Metric = c(
    "Total Return (%)",
    "Annualized Return (%)",
    "Annualized Volatility (%)",
    "Sharpe Ratio",
    "Sortino Ratio",
    "Maximum Drawdown (%)",
    "Calmar Ratio",
    "Win Rate (%)"
  ),
  Buy_and_Hold = c(
    round((tail(atgl_strat$Cum_Market, 1) - 1) * 100, 2),
    round(as.numeric(annual_ret[1, 1]) * 100, 2),
    round(as.numeric(annual_vol[1, 1]) * 100, 2),
    round(as.numeric(sharpe[1, 1]), 4),
    round(as.numeric(sortino[1, 1]), 4),
    round(as.numeric(max_dd[1]) * 100, 2),
    round(calmar_market, 4),
    NA
  ),
  SMA_Strategy = c(
    round((tail(atgl_strat$Cum_Strategy, 1) - 1) * 100, 2),
    round(as.numeric(annual_ret[1, 2]) * 100, 2),
    round(as.numeric(annual_vol[1, 2]) * 100, 2),
    round(as.numeric(sharpe[1, 2]), 4),
    round(as.numeric(sortino[1, 2]), 4),
    round(as.numeric(max_dd[2]) * 100, 2),
    round(calmar_strategy, 4),
    round(win_rate, 2)
  ),
  stringsAsFactors = FALSE
)
print(perf_summary, row.names = FALSE)
cat("\n")
# Save performance summary
write.csv(perf_summary, "Algo_Performance_Summary.csv", row.names = FALSE)
write_xlsx(perf_summary, "Algo_Performance_Summary.xlsx")
cat("Performance summary saved to CSV and Excel.\n\n")
# Drawdown Plot
cat("--- Creating Drawdown Plot ---\n")
png("Algo_Drawdown.png", width = 1200, height = 600, res = 150)
charts.PerformanceSummary(
  sma_xts,
  main = "Adani Gas - SMA Strategy Performance Summary",
  colorset = c("#3498db", "#e74c3c"),
  lwd = 1.5
)
dev.off()
cat("Performance summary chart saved.\n\n")
# Save Strategy Data
write.csv(atgl_strat %>% select(-SMA_Short_Label, -SMA_Long_Label),
          "Algo_SMA_Strategy_Data.csv", row.names = FALSE)
write.csv(atgl_rsi, "Algo_RSI_Strategy_Data.csv", row.names = FALSE)
cat("Strategy data saved.\n\n")
cat("=== PART 04: ALGORITHMIC TRADING COMPLETED ===\n\n")
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# PROJECT COMPLETE
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end_time <- Sys.time()
elapsed <- difftime(end_time, start_time, units = "mins")
cat("================================================================\n")
cat(" PROJECT COMPLETED!\n")
cat("================================================================\n")
cat(paste(" Execution Time:", round(as.numeric(elapsed), 2), "minutes\n\n"))
cat(" Output Files Generated:\n")
cat(" DATA:\n")
cat(" - Adani_Gas_Stock_Data.csv / .xlsx\n")
cat(" - Adani_Gas_Cleaned_Data.csv / .xlsx\n")
cat(" CHARTS (10):\n")
cat(" - Chart1_Closing_Price_Line.png\n")
cat(" - Chart2_Volume_Line.png\n")
cat(" - Chart3_Monthly_Avg_Close_Bar.png\n")
cat(" - Chart4_Yearly_Returns_Bar.png\n")
cat(" - Chart5_Candlestick.png\n")
cat(" - Chart6_Returns_Distribution.png\n")
cat(" - Chart7_Moving_Averages.png\n")
cat(" - Chart8_Monthly_Boxplot.png\n")
cat(" - Chart9_OHLC_Bar.png\n")
cat(" - Chart10_Price_Volume.png\n")
cat(" TIME SERIES:\n")
cat(" - TS_Trend_Analysis.png\n")
cat(" - TS_Decomposition_Multiplicative.png\n")
cat(" - TS_Decomposition_Additive.png\n")
cat(" - TS_STL_Decomposition.png\n")
cat(" - TS_ACF_Plot.png / TS_PACF_Plot.png\n")
cat(" - TS_ACF_Diff.png / TS_PACF_Diff.png\n")
cat(" - TS_ARIMA_Residuals.png\n")
cat(" - TS_ARIMA_Forecast.png\n")
cat(" - TS_Actual_vs_Forecast.png\n")
cat(" ALGORITHMIC TRADING:\n")
cat(" - Algo_SMA_Signals.png\n")
cat(" - Algo_Cumulative_Returns.png\n")
cat(" - Algo_RSI.png\n")
cat(" - Algo_RSI_Cumulative.png\n")
cat(" - Algo_Drawdown.png\n")
cat(" - Algo_Performance_Summary.csv / .xlsx\n")
cat(" - Algo_SMA_Strategy_Data.csv\n")
cat(" - Algo_RSI_Strategy_Data.csv\n")
cat("================================================================\n")

