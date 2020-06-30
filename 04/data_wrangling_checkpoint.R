# DATA WRANGLING OVERVIEW ----

bike_data_sizes_tbl <- readRDS("bike_data_sizes_tbl.rds")

# BEFORE WE START:
# Let's separate the category column (Slash, when it is not preceded or followd by a whitespace. Negative look ahead and negative look behind
bike_data_sizes_tbl <- bike_data_sizes_tbl %>%
    separate(col = category, into = c("category_1", "category_2", "category_3"), sep = "(?<!\\s)/(?!\\s)")

bike_data_sizes_tbl %>% glimpse()

# 1.0 Selecting Columns with select() ----

# Basic select (3 Ways, same result)
bike_data_sizes_tbl %>%
    select(name, id, category_1, category_2, category_3)

bike_data_sizes_tbl %>%
    select(1:5)

bike_data_sizes_tbl %>%
    select(1:5, starts_with("category_"))

# Reduce columns

bike_data_sizes_tbl %>%
    select(name, price_euro)

# Rearange columns

bike_data_sizes_tbl %>%
    select(id_size:stock_availability, everything())

# Select helpers

?starts_with

bike_data_sizes_tbl %>%
    select(starts_with("price"))

# pull(). Extracts contents of a tibble column

bike_data_sizes_tbl %>%
    # select(total_price) %>%
    pull(price_euro) %>%
    mean()

bike_data_sizes_tbl %>%
    pull(name)

# select_if

?select_if

bike_data_sizes_tbl %>%
    select_if(is.character)

bike_data_sizes_tbl %>%
    select_if(~ is.numeric(.))

bike_data_sizes_tbl %>%
    select_if(~ !is.numeric(.))


# 2.0 Arranging with arrange() and desc() ----

bike_data_sizes_tbl %>%
    select(name, price_euro) %>%
    arrange(desc(price_euro)) %>%
    View()





# 3.0 Filtering Rows with filter() ----

# 3.1 filter(): formula filtering ----

bike_data_sizes_tbl %>%
    select(name, price_euro) %>%
    filter(price_euro > mean(price_euro))

bike_data_sizes_tbl %>%
    select(name, price_euro) %>%
    filter((price_euro > 5000) | (price_euro < 1000)) %>%
    arrange(desc(price_euro)) %>%
    View()

bike_data_sizes_tbl %>%
    select(name, price_euro) %>%
    filter(price_euro > 6000,
           name %>% str_detect("Strive"))

# Filtering One or More Conditions Exactly Using == and %in%
bike_data_sizes_tbl %>%
    filter(category_1 %in% c("Hybrid / City", "E-Bikes"))

bike_data_sizes_tbl %>%
    filter(category_2 == "E-Mountain")

# Negated
bike_data_sizes_tbl %>%
    filter(category_2 != "E-Mountain")

bike_data_sizes_tbl %>%
    filter(!(category_2 %in% c("Hybrid / City", "E-Bikes")))


# 3.2 slice(): filtering with row number(s) ----

# First 5 rows
bike_data_sizes_tbl %>%
    arrange(desc(price_euro)) %>%
    slice(1:5)

bike_data_sizes_tbl %>%
    arrange(price_euro) %>%
    slice(1:5)

# Last 5 rows. nrow() returns number of rows in a tibble
bike_data_sizes_tbl %>%
    arrange(desc(price_euro)) %>%
    slice((nrow(.)-4):nrow(.))

# 3.3 distinct(): Unique values

bike_data_sizes_tbl %>%
    distinct(category_1)

bike_data_sizes_tbl %>%
    distinct(category_1, category_2)

bike_data_sizes_tbl %>%
    distinct(category_1, category_2, category_3)


# 4.0 Adding Columns with mutate() ----

# Adding column
bike_data_stock_tbl <- bike_data_sizes_tbl %>%
    select(name, size, stock_availability, price_euro) %>%
    mutate(total_stock_value = stock_availability * price_euro)

bike_data_stock_tbl

# Overwrite Column
bike_data_stock_tbl %>%
    mutate(total_stock_value = log(total_stock_value))

# Transformations
bike_data_stock_tbl %>%
    mutate(total_stock_value_log = log(total_stock_value)) %>%
    mutate(total_stock_value_sqrt = total_stock_value^0.5)

# Adding Flag (feature engineering)
bike_data_stock_tbl %>%
    mutate(is_strive = name %>% str_to_lower() %>% str_detect("strive")) %>%
    filter(is_strive)

# Binning with ntile()

bike_data_stock_tbl %>%
    mutate(total_stock_value_binned = ntile(total_stock_value, 3))

# case_when() - more flexible binning

# Numeric to Categorical
bike_data_stock_tbl %>%
    mutate(total_stock_value_binned = ntile(total_stock_value, 3)) %>%
    mutate(total_stock_value_binned2 = case_when(
        total_stock_value > quantile(total_stock_value, 0.75) ~ "High",
        total_stock_value > quantile(total_stock_value, 0.25) ~ "Medium",
        TRUE ~ "Low" # Everything else
    ))

# Text to Categorical
bike_data_stock_tbl %>%
    mutate(bike_type = case_when(
        name %>% str_to_lower() %>% str_detect("aeroad") ~ "Aeroad",
        name %>% str_to_lower() %>% str_detect("ultimate") ~ "Ultimate",
        TRUE ~ "Not Aeroad or Ultimate" # Everything else
    ))

# 5.0 Grouping & Summarizing with group_by() and summarize() ----


# Basics
bike_data_stock_tbl %>%
    summarise(
        stock_value = sum(price_euro * stock_availability)
    )

bike_data_sizes_tbl %>%
    group_by(category_1) %>%
    summarise(stock_value = sum(price_euro * stock_availability))

bike_data_sizes_tbl %>%
    group_by(category_1, category_2) %>%
    summarise(stock_value = sum(price_euro * stock_availability)) %>%
    # Always ungroup() after you summarize(). Left-over groups will cause diffictult-to-detect errors.
    ungroup() %>%
    arrange(desc(stock_value))

bike_data_sizes_tbl %>%
    group_by(category_1, category_2, year) %>%
    summarise(stock_value = sum(price_euro * stock_availability)) %>%
    ungroup() %>%
    arrange(desc(stock_value))

# Summary functions
bike_data_sizes_tbl %>%
    mutate(total_stock_value = price_euro * stock_availability) %>%
    filter(total_stock_value != 0) %>%
    group_by(category_1, category_2) %>%
    summarize(
        count = n(), # Frequency (number if observations) in a group
        avg   = mean(total_stock_value), # arithmetic average
        med   = median(total_stock_value), # value at 50th percentile
        sd    = sd(total_stock_value), # measure of spread
        min   = min(total_stock_value),
        max   = max(total_stock_value)
    ) %>%
    ungroup() %>%
    arrange(desc(count))

# summarize_all() - detect missing values

bike_data_missing <- bike_data_sizes_tbl %>%

    # Create total stock value column and insert missing values for demonstration
    mutate(total_stock_value = price_euro * stock_availability) %>%
    mutate(total_stock_value = c(rep(NA, 4), total_stock_value[5:nrow(.)]))

# detect missing (absolute)
bike_data_missing %>%
    summarise_all(~ sum(is.na(.)))

# detect missing (relative)
bike_data_missing %>%
    summarise_all(~ sum(is.na(.)) / length(.))

# Handling missing data
bike_data_missing %>%
    filter(!is.na(total_stock_value))


# 6.0 Renaming columns with rename() and set_names() ----

# 6.1 rename: One column at a time ----

bike_data_sizes_tbl %>%
    select(name, category_1, category_2, category_3, price_euro) %>%
    rename(
        Model           = name,
        `Bike Family`   = category_1,
        `Ride Style`    = category_2,
        `Bike Category` = category_3,
        `Price in Euro`= price_euro
    )

# 6.2 set_names: All columns at once ----

bike_data_sizes_tbl %>%
    select(name, category_1, category_2, category_3, price_euro) %>%
    set_names(c("Model", "Bike Family", "Ride Style", "Bike Category", "Price in Euro"))

bike_data_sizes_tbl %>%
    select(name, category_1, category_2, category_3, price_euro) %>%
    set_names(names(.) %>% str_replace("_", " ") %>% str_to_title())


# 7.0 Reshaping (Pivoting) Data with spread() and gather() ----

# Wide is Reader Friednly. People tend to read data as wide format, where columns are categories and the cell contents are values

# 7.1 spread(): Long to Wide ----
- <- bike_data_sizes_tbl %>%
    mutate(total_stock_value = price_dollar * stock_availability) %>%
    filter(!is.na(year)) %>%
    select(year, category_1, total_stock_value) %>%
    group_by(year, category_1) %>%
    summarise(total_stock_value = sum(total_stock_value))

bike_data_years_formatted_tbl <- bike_data_years_tbl %>%
                                    pivot_wider(names_from  = category_1,
                                                values_from = total_stock_value) %>%
                                    arrange(desc(Mountain)) %>%
                                    mutate(
                                        Mountain = scales::dollar(Mountain),
                                        Road     = scales::dollar(Road),
                                        `Hybrid / City` = scales::dollar(`Hybrid / City`),
                                        `E-Bikes` = scales::dollar(`E-Bikes`)
                                    )

bike_data_years_formatted_tbl

# 7.2 gather(): Wide to Long ----

bike_data_years_formatted_tbl %>%
    pivot_longer(cols           = c(names(.)[2:5]),
                 names_to       = "category_1",
                 values_to      = "total_stock_value",
                 values_drop_na = T) %>%
    mutate(total_stock_value =  total_stock_value %>% str_remove_all("\\$|,") %>% as.double())


# 8.0 Joining Data by Key(s) with left_join() (e.g. VLOOKUP in Excel) ----

bike_price_tbl <- bike_data_sizes_tbl %>% select(1:13)
bike_size_tbl  <- bike_data_sizes_tbl %>% select(13:15) %>% rename(id = id_size)

bike_price_tbl %>%
    left_join(y = bike_size_tbl, by = c("id_size" = "id"))

# 9.0 Binding Data by Row or by Column with bind_rows() and bind_col() ----

# 9.1 bind_cols() ----

bike_data_sizes_tbl %>%
    select(-contains("category")) %>%

    bind_cols(
        bike_data_sizes_tbl %>% select(category_1)
    )



# 9.2 bind_rows() ----

train_tbl <- bike_data_sizes_tbl %>%
    slice(1:(nrow(.)/2))

train_tbl

test_tbl <- bike_data_sizes_tbl %>%
    slice((nrow(.)/2 + 1):nrow(.))

test_tbl

train_tbl %>%
    bind_rows(test_tbl)


# 10 Separate & Unite ----

orders_tbl <- read_csv("olist_orders_dataset.csv")

orders_tbl %>%
    select(order.estimated.delivery.date) %>%
    mutate(order.estimated.delivery.date = as.character(order.estimated.delivery.date)) %>%

    # separate
    separate(col = order.estimated.delivery.date, into = c("year", "month", "day"), sep = "-", remove = FALSE) %>%

    mutate(
        year  = as.numeric(year),
        month = as.numeric(month),
        day   = as.numeric(day)
    ) %>%

    # unite
    unite(order_date_united, year, month, day, sep = "-", remove = FALSE) %>%
    mutate(order_date_united = as.Date(order_date_united))

