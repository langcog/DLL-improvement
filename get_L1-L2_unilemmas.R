require(tidyverse)
require(wordbankr)

en_wg <- get_item_data(language = "English (American)", form = "WG")
en_ws <- get_item_data(language = "English (American)", form = "WS")

sp_wg <- get_item_data(language = "Spanish (Mexican)", form = "WG")
sp_ws <- get_item_data(language = "Spanish (Mexican)", form = "WS")

en_items <- en_wg %>% filter(!is.na(uni_lemma)) %>%
  bind_rows(en_ws %>% filter(!is.na(uni_lemma))) %>%
  select(-language, -form, -type, -complexity_category) %>%
  rename(english = definition) %>%
  distinct(english, category, lexical_class, uni_lemma) %>% # item_id, num_item_id)
  mutate(uni_lemma = case_when(
    uni_lemma == "t-shirt" ~ "shirt",
    TRUE ~ uni_lemma
  ))
  
sp_items <- sp_wg %>% filter(!is.na(uni_lemma)) %>%
  bind_rows(sp_ws %>% filter(!is.na(uni_lemma))) %>%
  select(-language, -form, -type, -complexity_category, -category, -lexical_class) %>%
  rename(spanish=definition) %>%
  distinct(spanish, uni_lemma) %>% # item_id, num_item_id)
  mutate(uni_lemma = case_when(
    uni_lemma == "t-shirt" ~ "shirt",
    TRUE ~ uni_lemma
  ))
  
dict <- en_items %>% left_join(sp_items, by="uni_lemma") %>% # 743, but only 414 in both
  filter(!is.na(spanish), !is.na(english)) 



#dict <- replace_item(dict, "spanish", "brazos", "brazo")
#dict <- replace_item(dict, "spanish", "vaso", "vasos")
#dict <- replace_item(dict, "spanish", "orejas", "oreja")
#dict <- replace_item(dict, "spanish", "shh", "shhh")

#length(intersect(coefs$sp$definition, dict$spanish)) # 367
#setdiff(dict$spanish, coefs$sp$definition)

#setdiff(dict$english, coefs$en$definition) # in inside

# add item difficulties and differences to dict
dict <- dict %>% 
  left_join(coefs$en %>% select(d, definition), by=c("english"="definition")) %>% 
  rename(en_d = d) %>% 
  left_join(coefs$sp %>% select(d, definition), by=c("spanish"="definition")) %>%
  rename(sp_d = d) %>% 
  mutate(en_sp_d_diff = en_d - sp_d,
         d_diff_sq = (sp_d - en_d)^2)

# can we match more of these?
missing_items <- dict %>% filter(is.na(d_diff_sq))
# we should be able to get:
#  finish - acabar / acabar(se)
#  

dict <- dict %>% filter(!is.na(d_diff_sq))

save(dict, file="data/English-Spanish-dict.Rdata")