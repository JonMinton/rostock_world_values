rm(list = ls())

pacman::p_load(
    tidyverse,
    stringr
    )

dir("data/F00005811-WV6_Data_ascii_delimited_v_2016_01_01/")

dta <- read_csv(
    "data/F00005811-WV6_Data_ascii_delimited_v_2016_01_01/WV6_Data_ascii_delimited_v_2016_01_01.dat",
    col_names = F
    )

lbl <- readLines(
    "data/F00005811-WV6_Data_ascii_delimited_v_2016_01_01/WV6_Data_ascii_delimited_v_2016_01_01.sts"
    )

lbl <- lbl[-c(1:8)]
head(lbl)

lbl_df <- data_frame(
    raw = lbl
    ) %>% 
    mutate(
        full_label = str_extract(raw, pattern = "\\{.{1,}\\}"),
        full_label = str_replace_all(full_label, "[\\{|\\}]", ""),
        f_part = str_extract(raw, "\\([A-Z]{1}[0-9]{1,2}\\)") %>% str_replace_all("[\\(|\\)]", ""),
        sq_part = str_extract(raw, "\\t\\[{1}.*\\]{1}") %>% str_replace_all("[\\[|\\]]", ""),
        var_label = str_extract(raw, "^[ ]{1,}[^ ]{1,}") %>% str_trim
    ) %>% .[1:430,]

# Country labels 
data_frame(
    raw = lbl
) %>% 
    .[447:648,] -> raw_country

str_split_fixed(raw_country$raw, pattern= "\\'{1}", n = 2) -> tmp

tmp[,1] <- str_replace_all(tmp[,1], "[ ]", "") %>% as.numeric
tmp[,2] <- sapply(tmp[,2], str_replace, "'$", "") 

colnames(tmp) <- c("value", "label")
head(tmp)
country_lookup <- as_data_frame(tmp)
rm(tmp)


names(dta) <- lbl_df$var_label

dta_demo <- dta %>% select(V1:V3, V229:V257)
dta_resp <- dta %>% select(V4:V228K, MN_35A:MN_249A3)

dta_demo %>% 
    select(
        country = V2,
        interview = V3,
        sex = V240,
        age = V242
        ) -> simple_demo

dta_resp_simple_demo <- bind_cols(
    simple_demo, dta_resp
    ) %>% 
    gather(V4:MN_249A3, key = "question", value = "response") %>% 
    mutate(response = ifelse(response < 0, NA, response)) %>% 
    filter(str_detect(question, "^V")) %>% 
    mutate(response = ifelse(response > 4, NA, response)) %>% 
    filter(sex > 0)

dta_resp_simple_demo %>% 
    group_by(country, sex, question, response) %>% 
    tally %>% 
    filter(!is.na(response)) %>% 
    spread(sex, n)

dta_resp_simple_demo %>%
    group_by(country, sex, question, response) %>%
    tally %>%
    filter(!is.na(response)) %>%
    spread(sex, n) %>% 
    mutate(mf = `1` / `2`) %>% 
    group_by(country, question) %>% 
    summarise(
        agree_male = sum(`1`[response < 3]), 
        disagree_male = sum(`1`[response > 2]), 
        agree_female = sum(`2`[response < 3]), 
        disagree_female = sum(`2`[response > 2])) %>% 
    mutate(
        odds = (agree_male / disagree_male) / (agree_female / disagree_female)
        ) -> agree_disagree

agree_disagree %>% left_join(country_lookup, by = c("country" = "value"))

country_lookup %>% mutate(value = as.numeric(value)) %>% right_join(agree_disagree, by = c("value" = "country")) -> tmp

lbl_df %>% select(var_label, full_label) -> tmp2
tmp %>% 
    left_join(tmp2, by = c("question" = "var_label")) %>% 
    select(country = label, question = full_label, odds
           ) -> agree_odds

ggplot(
    agree_odds, 
    aes(y = question, x= country, fill = log(odds))
    ) + geom_tile() + 
    scale_fill_distiller(palette = "RdBu")


dta_resp_simple_demo %>%
    group_by(country, question, response) %>%
    tally %>%
    filter(!is.na(response)) %>% 
    mutate(flag = response == 1 | response == 2) %>% 
    group_by(country, question) %>% 
    summarise(
        total_agree = sum(n[flag == T]),
        total = sum(n)
              ) %>% 
    mutate(prop_agree = total_agree / total) -> agree_prop

ggplot(agree_prop, aes(x = as.factor(country), y= question, fill = prop_agree)) + 
    geom_tile() + 
    scale_fill_distiller(palette = "Spectral")
