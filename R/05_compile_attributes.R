# =============================================================================
# Check that necessary objects exist
# =============================================================================

objects_needed <- c(
    "hholds",
    "members",
    "food_df",
    "assets",
    "parcels",
    "plots",
    "cases_full_interview",
    "cases_to_review",
    "cases_single_phase"
)

check_exists(objects_needed)

# =============================================================================
# Load necessary libraries
# =============================================================================

library(dplyr)
library(susoreview)
library(purrr)
library(rlang)

# =============================================================================
# Create attributes
# =============================================================================

# -----------------------------------------------------------------------------
# Household
# -----------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Enterprise
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# run a business
attrib_run_business <- cases_full_interview %>%
    dplyr::left_join(hholds, by = c("interview__id", "interview__key")) %>%
    susoreview::create_attribute(
        condition = HH_enterprise_check == 1,
        attrib_name = "run_business",
        attrib_vars = "^NCF0[1-7]"
    )

# number of businesses
attrib_num_businesses <- cases_full_interview %>%
    dplyr::left_join(hholds, by = c("interview__id", "interview__key")) %>%
    susoreview::count_list(
        var_pattern = "NCE00__",
        attrib_name = "num_businesses",
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Agriculture
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# engaged in agriculture
attrib_raises_crops <- cases_to_review %>%
    dplyr::left_join(hholds, by = c("interview__id", "interview__key")) %>%
    susoreview::create_attribute(
        condition = s19q01 == 1,
        attrib_name = "raise_crops",
        attrib_vars = "^s19q01"
    )

# intend to be engaged in ag
attrib_intend_raise_crops <- cases_to_review %>%
    dplyr::left_join(hholds, by = c("interview__id", "interview__key")) %>%
    susoreview::create_attribute(
        condition = s19q02 == 1,
        attrib_name = "intend_raise_crops",
        attrib_vars = "^s19q02"
    )

# engaged in livestock
attrib_livestock <- cases_to_review %>%
    dplyr::left_join(hholds, by = c("interview__id", "interview__key")) %>%
    susoreview::create_attribute(
        condition = s19q03 == 1,
        attrib_name = "raise_livestock",
        attrib_vars = "^s19q03"
    )

# intend to be engaged in livestock
attrib_intend_livestock <- cases_to_review %>%
    dplyr::left_join(hholds, by = c("interview__id", "interview__key")) %>%
    susoreview::create_attribute(
        condition = s19q04 == 1,
        attrib_name = "intend_raise_livestock",
        attrib_vars = "^s19q04"
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Food at home
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# number of food items consumed at home
attrib_num_food_items <- cases_full_interview %>%
    dplyr::left_join(hholds, by = c("interview__id", "interview__key")) %>%
    susoreview::count_vars(
        var_pattern = "S(7|15)B1", # TODO: remove S15 before deployment
        attrib_name = "num_food_items"
    ) 

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Non-food consumption
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

attrib_non_food_consumption <- cases_full_interview %>%
    dplyr::left_join(hholds, by = c("interview__id", "interview__key")) %>%
    susoreview::count_vars(
        var_pattern = "Item_grp[1-3]_30days",
        attrib_name = "non_food_consumption"
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Non-hhold ag labor inputs
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

attrib_any_non_hh_ag_labor <- cases_full_interview %>%
    dplyr::left_join(hholds, by = c("interview__id", "interview__key")) %>%
    susoreview::count_vars(
        var_pattern = "ah3bq01",
        attrib_name = "any_non_hh_ag_labor"
    )

# -----------------------------------------------------------------------------
# Member
# -----------------------------------------------------------------------------

# number of household heads
attrib_num_heads <- cases_to_review %>%
    dplyr::left_join(members, by = c("interview__id", "interview__key")) %>%
    susoreview::count_obs(
        where = HR07 == 1,
        attrib_name = "num_heads",
        attrib_vars = "HR07"
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Labor activities
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# work on household farm
attrib_work_on_farm <- cases_to_review %>%
    dplyr::left_join(members, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = LF07 == 1,
        attrib_name = "work_on_farm",
        attrib_vars = "LF07"
    )

# work in household enterprise
attrib_work_in_hh_business <- cases_to_review  %>%
    dplyr::left_join(members, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = (LF04 == 1 | LF05 ==1),
        attrib_name = "work_in_hh_business",
        attrib_vars = "LF0[45]"
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Food away from home
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

attrib_food_away_from_home <- cases_full_interview %>%
    dplyr::left_join(members, by = c("interview__id", "interview__key")) %>%
    dplyr::mutate(
        food_away_from_home = dplyr::if_any(
            .cols = c(FC02, FC05, FC07, FC09, FC11, FC13, FC15),
            .fns = ~ .x == 1
        )
    ) %>%
    susoreview::any_obs(
        where = food_away_from_home == 1,
        attrib_name = "food_away_from_home",
        attrib_vars = "FC02|FC05|FC07|FC09|FC11|FC13|FC15"
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Household ag labor inputs
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# any household labor inputs
attrib_any_hh_ag_labor <- cases_to_review %>%
    dplyr::left_join(members, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = ah3aq02_s2 == 1,
        attrib_name = "any_hh_ag_labor",
        attrib_vars = "ah3aq02_s2"
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Woman nutrition
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

vals <- c(
    2, # GRAINS AND CEREALS: Rice, roti, chapati, bread, puffed rice, maize/corn, pressed rice, noodles, millet, porridge, wheat, buckwheat, sorghum or other foods made from grains....
    3, # WHITE TUBERS AND ROOTS OR OTHER STARCHY FOOD: Matooke, Irish potatoes, cassava, white yams, white or yellow sweet potato (NOT ORANGE INSIDE), or other foods made from roots...
    4, # Beans, peas, or lentils or any foods made from these?..........................................................................................................................
    5, # Nuts and seeds or any foods made from these (groundnut, cashew, sesame, etc)  If less than 15 g (<1 TBS), code as condiment....................................................
    6, # MILK AND MILK PRODUCTS: Milk, cheese, eshabwe, yogurt, or other food made from milk............................................................................................
    7, # EGGS: Chicken, duck, quail, turkey, etc........................................................................................................................................
    8, # ORGAN MEAT: Liver, kidney, heart, offal, or other organ meats..................................................................................................................
    9, # OTHER MEAT: Beef, lamb, goat, chicken, duck, rabbit, pork, game meat, turkey, sausage..........................................................................................
    10, # FISH: Big/small fresh or dried or shellfish such as prawn, crab, silverfish, etc...............................................................................................
    11, # DARK GREEN LEAFY VEGETABLES: spinach, amaranth leaves, mustard leaves, pumpkin leaves, dodo, other.............................................................................
    121, # VITAMIN A RICH VEGETABLES AND TUBERS: Pumpkin, carrots, squash, orange sweet potato............................................................................................
    122, # -- CHANGE -- # ORANGE SWEET POTATO: sweet potatoes that are orange inside (SHOW EXAMPLE PHOTOGRAPH)
    13, # OTHER VEGETABLES: Cauliflower, broccoli, cabbage, eggplant, green papaya, radish, onion, tomatoes, cucumber, zucchini, green peppers, etc......................................
    14, # VITAMIN A RICH FRUITS: Ripe mangoes, ripe papayas, red palm fruit/pulp (kinazi), passion fruit, matungunda.....................................................................
    15, # OTHER FRUITS: Bananas, apples, guavas, oranges, other citrus fruits, pineapple, watermelon, grapes, strawberries, plum, jackfruit, gooseberry, tamarind, avocado, etc..........
    16, # SWEETS: Sugar, honey, rock candy, chocolates,  biscuits, jams, cakes, pastries.................................................................................................
    18, # Any purchased sweet drinks with sugar (juice drinks with added sugar, fizzy drinks, soda)? (Excludes diet soda)................................................................
    241, # -- CHANGE --# # Cooking/edible fat (e.g., KIMBO, Cowboy, Butter, vegetable or animal ghee, shea butter, lard)
    242, # -- CHANGE --# # Cooking oil (other than red palm oil)
    281, # FRIED/SALTY FOODS PURCHASED OUTSIDE THE HOME:  (fried/salty foods that are purchased including fast foods from restaurants; packaged salty snacks  (chips, puffs, or crisps); deep-fried foods (dough/bread, donuts, samosas); instant noodles
    29 # CONDIMENTS/SEASONINGS: (foods used in small amounts for flavoring) chilies, spices, herbs, fish powder, tomato paste, seeds, flavor cubes, etc.................................
)

attribs <- c(
    "woman24h_grains",
    "woman24h_tubers",
    "woman24h_legumes",
    "woman24h_nuts",
    "woman24h_milk_prod",
    "woman24h_eggs",
    "woman24h_organ_meat",
    "woman24h_oth_meat",
    "woman24h_fish",
    "woman24h_leafy_veg",
    "woman24h_vit_a_veg",
    "woman24h_orange_potatoes",
    "woman24h_oth_veg",
    "woman24h_vit_a_fruit",
    "woman24h_oth_fruit",
    "woman24h_sweets",
    "woman24h_purch_sug_bev",
    "woman24h_fats",
    "woman24h_oils",
    "woman24h_salty_snacks",
    "woman24h_condiments"
)

attrib_woman_24h <- purrr::map2(
        .x = vals,
        .y = attribs,
        .f = ~ susoreview::any_obs(
            df = dplyr::left_join(cases_full_interview, members, by = c("interview__id", "interview__key")),
            where = !!rlang::parse_quo(
                paste0("ws7q02__", .x , " == 1"), 
                rlang::global_env()
            ),
            attrib_name = .y,
            attrib_vars = paste0("ws7q02__", .x)
        )
    ) %>% 
    dplyr::bind_rows()

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Child nutrition
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

vals <- c(
    1, # Porridge (posho), bread (chapatti, pancakes), rice, noodles, pasta, or foods made from millet, sorghum,cassava/cassava flour, maize, buckwheat, or other grains (e.g., weetabix, cornflakes)
    2, # Pumpkin, carrots, sweet red peppers ,squash or sweet potatoes that are dark yellow or orange inside
    3, # Plantains, matooke, white/Irish potatoes, white yams, pale yellow sweet potato  , unripe bananas
    4, # Dark green leafy vegetables like eboo, amaranth, cowpea leaves, pumpkin leaves etc
    5, # Any other vegetables, such as avocado, tomato, okra, cucumber, peas, green beans, fresh corn, mushroom, green pepper
    6, # Orange-flesh fruits like ripe mangoes, ripe papayas, or red palm fruit, red palm nut or red palm nut pulp sauce
    7, # Any other fruits (apples, unripe mango, unripe papaya, ripe bananas, oranges, tamarind, blackberry, gooseberry, jackfruit, soursop etc)
    8, # Liver, kidney, heart, or other organ meats
    9, # Sausages, hot dogs, ham, bacon, salami, canned meat or other processed meats
    10, # Any other meat such as beef, pork, lamb, goat, chicken, or duck, bush meat (probe if meat taken in soup)
    11, # Eggs
    12, # Fresh or dried fish, tilapia, Nile perch or other seafood, shellfish
    13, # Beans, peas, lentils, groundnuts, soya, other nuts, sesame, chickpea or other seeds
    14, # Yogurt, other than yogurt drinks
    15, # Hard or soft cheese (cheddar, paneer, mozzarella, etc)
    16, # Sweet foods such as chocolates, candies, pastries, cakes, pies, biscuits, or frozen treats (ice cream, popsicles)
    20  # Foods made with other fats or oils, such as butter, ghee, lard (animal fats), margarine, vegetable/fruit/nut/seed oils (maize, canola, groundnut, olive, coconut, safflower, etc)
)

attribs <- c(
    "child24h_grains",
    "child24h_color_veg",
    "child24h_white_root",
    "child24h_leafy_veg",
    "child24h_oth_veg",
    "child24h_orange_fruit",
    "child24h_oth_fruit",
    "child24h_organ_meat",
    "child24h_process_meat",
    "child24h_oth_meat",
    "child24h_eggs",
    "child24h_fish",
    "child24h_legumes",
    "child24h_yoghurt",
    "child24h_cheese",
    "child24h_sweets",
    "child24h_oils"
)

attrib_child_24h <- purrr::map2(
        .x = vals,
        .y = attribs,
        .f = ~ susoreview::any_obs(
            df = dplyr::left_join(cases_full_interview, members, by = c("interview__id", "interview__key")),
            where = !!rlang::parse_quo(
                paste0("CHN14__", .x , "== 1"), 
                rlang::global_env()
            ),
            attrib_name = .y,
            attrib_vars = paste0("CHN14__", .x)
        )
    ) %>%
    dplyr::bind_rows()

# -----------------------------------------------------------------------------
# Food consumption
# -----------------------------------------------------------------------------

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Missing quantity, unit, or size for food item
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

attrib_food_item_missing_info <- food_df %>%
    susoreview::any_obs(
        where = (
            haven::is_tagged_na(CEB05) |
            haven::is_tagged_na(CEB06) |
            haven::is_tagged_na(CEB07) |
            haven::is_tagged_na(CEB10) |
            haven::is_tagged_na(CEB12)
        ),
        attrib_name = "food_item_missing_info",
        attrib_vars = "CEB0[567]|CEB1[02]"
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Items from woman 24h recall
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# grains
grain_vals <- c(
    # GRAINS AND CEREALS: 
    104, # Rice, 
    # roti, 
    110,  # chapati, 
    109, # bread, 
    115, # puffed rice, 
    101, 102, # maize/corn, 
    # pressed rice, 
    108, # noodles, 
    # millet, 
    # porridge, 
    # wheat, 
    # buckwheat, 
    164, # cassava/cassava flour,
    107, # sorghum or 
    105, 106, 111, 112, 113, 114, 116, 117 # other foods made from grains
)

attrib_w_conso7d_grains <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% grain_vals,
        attrib_name = "w_conso7d_grains",
        attrib_vars = "S7B1_(Cereals|Starches)"
    )

# tubers
tuber_vals <- c(
    # WHITE TUBERS AND ROOTS OR OTHER STARCHY FOOD: 
    153:157, # Matooke, 
    167, # Irish potatoes, 
    165, # white yams, 
    160, 161, # white or yellow sweet potato (NOT ORANGE INSIDE), 
    162, 163, 164, 169, 170 # or other foods made from roots
)

attrib_w_conso7d_tubers <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% tuber_vals,
        attrib_name = "w_conso7d_tubers",
        attrib_vars = "S7B1_Starches"
    )

# legumes
legume_vals <- c(
    171, 172, 173, 174, # Beans, 
    179, 180, # peas, or 
    # lentils or 
    183 # any foods made from these?
)

attrib_w_conso7d_legumes <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% legume_vals,
        attrib_name = "w_conso7d_legumes",
        attrib_vars = "S7B1_Pulses"
    )

# nuts, seeds
nuts_vals <- c(
    183, # Nuts and seeds or any foods made from these (
    175:178, # groundnut, 
    # cashew, 
    181, 182 # sesame, etc)-If less than 15 g (<1 TBS), code as condiment
)

attrib_w_conso7d_nuts <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% nuts_vals,
        attrib_name = "w_conso7d_nuts",
        attrib_vars = "S7B1_Pulses"
    )

# milk products
milk_vals <- c(
    # MILK AND MILK PRODUCTS: 
    137, 138, 139, # Milk, 
    215, # cheese, 
    214, # eshabwe, 
    140, # yogurt, or 
    141, 142, 144 # other food made from milk
)

attrib_w_conso7d_milk_prod <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% milk_vals,
        attrib_name = "w_conso7d_milk_prod",
        attrib_vars = "S7B1_(Meats|Oils)"
    )

# eggs
egg_vals <- c(
    # EGGS: Chicken, duck, quail, turkey, etc
    143
)

attrib_w_conso7d_eggs <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% egg_vals,
        attrib_name = "w_conso7d_egg",
        attrib_vars = "S7B1_Meats"
    )

# organ meat
organ_vals <- c(
    # ORGAN MEAT: 
    119, 123, # Liver, 
    120, 124 # kidney, heart, offal, or other organ meats    
)

attrib_w_conso7d_organ_meat <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% organ_vals,
        attrib_name = "w_conso7d_organ_meat",
        attrib_vars = "S7B1_Meats"
    )

# other meat
meat_vals <- c(
    # OTHER MEAT: 
    118, 121, # Beef, 
    128, # lamb, 
    122, 125, # goat, 
    131:135, # chicken, 
    # duck, 
    # rabbit, 
    126, 127, # pork, 
    # game meat, 
    # turkey, 
    129, # sausage
    130, 136
)

attrib_w_conso7d_oth_meat <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% meat_vals,
        attrib_name = "w_conso7d_oth_meat",
        attrib_vars = "S7B1_Meats"
    )

# fish
fish_vals <- c(
    # FISH: 
    145, 146, 151, # Big/small fresh or 
    147:149, 152, # dried or 
    150 # shellfish such as prawn, crab, silverfish, etc
)

attrib_w_conso7d_fish <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% fish_vals,
        attrib_name = "w_conso7d_fish",
        attrib_vars = "S7B1_Meats"
    )

# leafy vegetables
leafy_veg_vals <- c(
    #DARK GREEN LEAFY VEGETABLES: 
    # spinach, 
    # amaranth leaves, 
    # mustard leaves, 
    # pumpkin leaves, 
    188, # dodo, 
    191, 197 # other
)

attrib_w_conso7d_leafy_veg <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% leafy_veg_vals,
        attrib_name = "w_conso7d_leafy_veg",
        attrib_vars = "S7B1_Veges"
    )

# vitamin A rich vegetables and tubers
vit_a_veg_vals <- c(
    # VITAMIN A RICH VEGETABLES AND TUBERS: 
    190, # Pumpkin, 
    195, # carrots, 
    197 # squash (Do NOT include orange sweet potato)    
)

attrib_w_conso7d_vit_a_veg <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% vit_a_veg_vals,
        attrib_name = "w_conso7d_vit_a_veg",
        attrib_vars = "S7B1_Veges"
    )

# other vegetables
oth_veg_vals <- c(
    # OTHER VEGETABLES: 
    # Cauliflower, 
    # broccoli, 
    187, # cabbage, 
    196, # eggplant, 
    # green papaya, 
    # radish, 
    184, # onion, 
    186, # tomatoes, 
    193, # cucumber, 
    # zucchini, 
    189,# green peppers, 
    192, # mushrooms 
    194, 197 # etc    
)

attrib_w_conso7d_oth_veg <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% oth_veg_vals,
        attrib_name = "w_conso7d_oth_veg",
        attrib_vars = "S7B1_(Veges|Oils)"
    )

# vitamin A rich fruits
vit_a_fruit_vals <- c(
    # VITAMIN A RICH FRUITS: 
    199, # Ripe mangoes, 
    # ripe papayas, 
    # red palm fruit/pulp (kinazi), 
    198, # passion fruit, 
    # matungunda
    208
)

attrib_w_conso7d_vit_a_fruit <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% vit_a_fruit_vals,
        attrib_name = "w_conso7d_vit_a_fruit",
        attrib_vars = "S7B1_Veges"
    )

# other fruit
oth_fruit_vals <- c(
    # OTHER FRUITS: 
    158, 159, # Bananas, 
    204, # apples, 
    206, # guavas, 
    200, # oranges, other citrus fruits, 
    202, # pineapple, 
    201, # watermelon, 
    # grapes, 
    # strawberries, 
    # plum, 
    205, # jackfruit, 
    # gooseberry, 
    # tamarind, 
    207, # avocado, 
    # soursop/kitaferi  
    203, 208 # etc    
)

attrib_w_conso7d_oth_fruit <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% oth_fruit_vals,
        attrib_name = "w_conso7d_oth_fruit",
        attrib_vars = "S7B1_(Veges|Starches)"
    )

# sweets
sweets_vals <- c(
    # SWEETS: 
    209, # Sugar, 
    218, # honey, 
    # rock candy, chocolates,  
    112, # biscuits, 
    # jams, 
    113, # cakes, 
    # pastries
    219
)

attrib_w_conso7d_sweets <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% sweets_vals,
        attrib_name = "w_conso7d_sweets",
        attrib_vars = "S7B1_(Cereals|Oils)"
    )

# purchased sugary drinks
sugary_bev_vals <- c(
    # Any purchased sweet drinks with sugar (juice drinks with added sugar, fizzy drinks, soda)? (Excludes diet soda)
    224, 225, 229, 231
)

attrib_w_conso7d_sugary_bev <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% sugary_bev_vals,
        attrib_name = "w_conso7d_sugary_bev",
        attrib_vars = "S7B1_Bev"
    )

# fats
fats_vals <- c(
    # Cooking/edible fat (e.g., 
    213, # KIMBO, Cowboy, 
    217, # Butter, 
    214, 216, # vegetable or animal ghee, 
    # shea butter, 
    # lard)
    219
)

attrib_w_conso7d_fats <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% fats_vals,
        attrib_name = "w_conso7d_fats",
        attrib_vars = "S7B1_Oils"
    )

# cooking oil other than red palm oil
oils_vals <- c(
    211, 212, # Cooking oil (other than red palm oil)
    219
)

attrib_w_conso7d_oils <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% oils_vals,
        attrib_name = "w_conso7d_oils",
        attrib_vars = "S7B1_Oils"
    )

# fried/salty foods inside home
savory_snack_vals <- c(
    # FRIED/SALTY FOODS PREPARED INSIDE THE HOME: (deep-fried/salty foods (
    169, # potato/maize/corn chips, puffs, or crisps; 
    166, # fried dough/bread, 
    114, # donuts, 
    111, # samosas); 
    108, # prepackaged-instant noodles
    117, 170
)

attrib_w_conso7d_savory_snack <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% savory_snack_vals,
        attrib_name = "w_conso7d_savory_snack",
        attrib_vars = "S7B1_(Cereals|Starches)"
    )

# condiments
condiment_vals <- c(
    # CONDIMENTS/SEASONINGS: (foods used in small amounts for flavoring) 
    # chilies, 
    185, 232, 233, 234, # spices, herbs, fish powder, 
    236, # tomato paste, 
    # seeds, flavor cubes, 
    237 # etc
)

attrib_w_conso7d_condiment <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% condiment_vals,
        attrib_name = "w_conso7d_condiment",
        attrib_vars = "S7B1_Oils"
    )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Items from child 24h recall
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# cereals
cereal_vals <- c(
    # Porridge (posho), 
    109, 110, # bread (chapatti, pancakes), 
    104, # rice, 
    108, # noodles, pasta, or 
    105, # foods made from millet, 
    107, 164, # sorghum,cassava/cassava flour, 
    101:103, # maize, 
    # buckwheat, 
    115, 116, 117 # or other grains (e.g., weetabix, cornflakes)
)

attrib_c_conso7d_cereal <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% cereal_vals,
        attrib_name = "c_conso7d_cereal",
        attrib_vars = "S7B1_(Cereals|Starches)"
    )

# vitamin A rich vegetables and tubers
vit_a_veg_vals <- c(
    190, # Pumpkin, 
    195, # carrots, 
    197, # sweet red peppers ,
    # squash or 
    160, 161 # sweet potatoes that are dark yellow or orange inside
)

attrib_c_conso7d_vit_a_veg <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% vit_a_veg_vals,
        attrib_name = "c_conso7d_vit_a_veg",
        attrib_vars = "S7B1_(Veges|Starches)"
    )


# tubers
tuber_vals <- c(
    # Plantains, 
    153:157, # matooke, 
    167, # white/Irish potatoes, 
    165, # white yams, 
    160, # pale yellow sweet potato  , 
    158, 159 # unripe bananas
)

attrib_c_conso7d_tubers <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% tuber_vals,
        attrib_name = "c_conso7d_tubers",
        attrib_vars = "S7B1_Starches"
    )

# leafy vegetables
leafy_veg_vals <- c(
    # Dark green leafy vegetables like 
    # eboo, 
    # amaranth, 
    # cowpea leaves, 
    191, 197, 188 # pumpkin leaves etc
)

attrib_c_conso7d_leafy_veg <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% leafy_veg_vals,
        attrib_name = "c_conso7d_leafy_veg",
        attrib_vars = "S7B1_Veges"
    )


# other vegetables
oth_veg_vals <- c(
    # Any other vegetables, such as 
    207, # avocado, 
    186, # tomato, 
    194, # okra, 
    193, # cucumber, 
    179, 180, # peas, 
    171, 172, # green beans, 
    102, # fresh corn, 
    192, # mushroom, 
    189 # green pepper
)

attrib_c_conso7d_oth_veg <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% oth_veg_vals,
        attrib_name = "c_conso7d_oth_veg",
        attrib_vars = "S7B1_(Veges|Pulses|Cereals)"
    )

# orange fruit
orange_fruit_vals <- c(
    # Orange-flesh fruits like 
    199, # ripe mangoes, 
    208 # ripe papayas, or 
    # red palm fruit, red palm nut or red palm nut pulp sauce
)

attrib_c_conso7d_orange_fruit <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% orange_fruit_vals,
        attrib_name = "c_conso7d_orange_fruit",
        attrib_vars = "S7B1_Veges"
    )


# other fruit
oth_fruit_vals <- c(
    # Any other fruits 
    204, # (apples, 
    199, # unripe mango, 
    # unripe papaya, 
    158, 159, # ripe bananas, 
    200, # oranges, 
    # tamarind, blackberry, gooseberry, 
    205, # jackfruit, 
    208, 206, 202, 201, 207 # soursop etc)
)

attrib_c_conso7d_oth_fruit <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% oth_fruit_vals,
        attrib_name = "c_conso7d_oth_fruit",
        attrib_vars = "S7B1_(Veges|Starches)"
    )


# organ meat
organ_vals <- c(
    119, 123, # Liver, 
    120, 124 # kidney, heart, or other organ meats
)

attrib_c_conso7d_organ_meat <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% organ_vals,
        attrib_name = "c_conso7d_organ_meat",
        attrib_vars = "S7B1_Meats"
    )

# processed meat
process_meat_vals <- c(
    # Sausages, hot dogs, ham, bacon, salami, canned meat or other processed meats
    129, 136
)

attrib_c_conso7d_process_meat <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% process_meat_vals,
        attrib_name = "c_conso7d_process_meat",
        attrib_vars = "S7B1_Meats"
    )


# other meat
oth_meat_vals <- c(
    # Any other meat such as 
    118, 121, # beef, 
    126, 127, # pork, 
    128, # lamb, 
    122, 125, # goat, 
    131:135, # chicken, or 
    # duck, 
    # bush meat (probe if meat taken in soup)
    130, 136
)

attrib_c_conso7d_oth_meat <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% oth_meat_vals,
        attrib_name = "c_conso7d_oth_meat",
        attrib_vars = "S7B1_Meats"
    )


# eggs
eggs_vals <- c(
    # Eggs
    143
)

attrib_c_conso7d_eggs <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% eggs_vals,
        attrib_name = "c_conso7d_eggs",
        attrib_vars = "S7B1_Meats"
    )


# fish
fish_vals <- c(
    # Fresh or dried fish, tilapia, Nile perch or 
    # other seafood, shellfish
    145:152
)

attrib_c_conso7d_fish <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% fish_vals,
        attrib_name = "c_conso7d_fish",
        attrib_vars = "S7B1_Meats"
    )

# legumes
legume_vals <- c(
    171, 172, # Beans, 
    179, 180, # peas, 
    # lentils, 
    175:178, # groundnuts, 
    173, 174, # soya, 
    181:183 # other nuts, sesame, chickpea or other seeds
)

attrib_c_conso7d_legumes <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% legume_vals,
        attrib_name = "c_conso7d_legumes",
        attrib_vars = "S7B1_Pulses"
    )


# yoghurt
yoghurt_vals <- c(
    # Yogurt, other than yogurt drinks
    139, 140, 144
)

attrib_c_conso7d_yoghurt <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% yoghurt_vals,
        attrib_name = "c_conso7d_yoghurt",
        attrib_vars = "S7B1_Meats"
    )


# cheese
cheese_vals <- c(
    # Hard or soft cheese (cheddar, paneer, mozzarella, etc)
    215
)

attrib_c_conso7d_cheese <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% cheese_vals,
        attrib_name = "c_conso7d_cheese",
        attrib_vars = "S7B1_Meats"
    )


# oils
oils_vals <- c(
    # Foods made with 
    # other fats or 
    211, 212, # oils, such as 
    217, # butter, 
    214, # ghee, 
    213, # lard (animal fats), 
    216, # margarine, 
    219  # vegetable/fruit/nut/seed oils (maize, canola, groundnut, olive, coconut, safflower, etc)
)

attrib_c_conso7d_oils <- cases_full_interview %>%
    dplyr::left_join(food_df, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = food__id %in% oils_vals,
        attrib_name = "c_conso7d_oils",
        attrib_vars = "S7B1_Oils"
    )

# -----------------------------------------------------------------------------
# Assets
# -----------------------------------------------------------------------------

owns_asset_vals <- c(1, 4, 5)

# number of assets owned
attrib_num_assets <- cases_full_interview %>%
    dplyr::left_join(assets, by = c("interview__id", "interview__key")) %>%
    susoreview::count_obs(
        where = HA03 %in% owns_asset_vals,
        attrib_name = "num_assets",
        attrib_vars = "HA03"
    )

# owns owner-occupied home
attrib_owns_home <- cases_full_interview %>%
    dplyr::left_join(assets, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = (HA03 %in% owns_asset_vals) & (HH_Assets__id == 1),
        attrib_name = "owns_home",
        attrib_vars = "HA03"
    )

# owns non-ag land
attrib_owns_non_ag_land <- cases_full_interview %>%
    dplyr::left_join(assets, by = c("interview__id", "interview__key")) %>%
    susoreview::any_obs(
        where = (HA03 %in% owns_asset_vals) & (HH_Assets__id == 3),
        attrib_name = "owns_non_ag_land",
        attrib_vars = "HA03"        
    )

# -----------------------------------------------------------------------------
# Parcels
# -----------------------------------------------------------------------------

parcel_use <- cases_to_review %>%
    dplyr::left_join(parcels, by = c("interview__id", "interview__key")) %>%
    dplyr::select(interview__id, interview__key, dplyr::starts_with("hp2q21_1"))

# number of ag parcels
attrib_num_ag_parcels <- parcel_use %>%
    susoreview::count_obs(
        where = hp2q21_1__2 >= 1,
        attrib_name = "num_ag_parcels",
        attrib_vars = "hp2q21_1"
    )

# number of crop grazing parcels
attrib_num_grazing_parcels <- parcel_use %>%
    susoreview::any_obs(
        where = hp2q21_1__3 >= 1,
        attrib_name = "num_grazing_parcels",
        attrib_vars = "hp2q21_1"
    )

# any land for business
attrib_parcel_for_business <- parcel_use %>%
    susoreview::any_obs(
        where = hp2q21_1__8 >= 1,
        attrib_name = "parcel_for_business",
        attrib_vars = "hp2q21_1"
    )

# any land for home
attrib_parcel_for_home <- parcel_use %>%
    susoreview::any_obs(
        where = hp2q21_1__1 >= 1,
        attrib_name = "parcel_for_home",
        attrib_vars = "hp2q21_1"
    )

# any non-ag land
attrib_parcel_for_non_ag <- parcel_use %>%
    susoreview::any_obs(
        where = hp2q21_1__5 >= 1 | hp2q21_1__7 >= 1 | hp2q21_1__8 >= 1 | hp2q21_1__9 >= 1 | hp2q21_1__11 >= 1 | hp2q21_1__n96 >= 1,
        attrib_name = "parcel_for_non_ag",
        attrib_vars = "hp2q21_1"
    )

# -----------------------------------------------------------------------------
# Plots
# -----------------------------------------------------------------------------

# number of planted plots
attrib_num_planted_plots <- plots %>%
    susoreview::count_obs(
        where = a3q05 == 1 | a3q05 == 2,
        attrib_name = "num_planted_plots",
        attrib_vars = "a3q05" 
    )

# number of plots with crops
plots_w_indicators <- plots %>%
    dplyr::mutate(
        dplyr::across(
            .cols = tidyselect::starts_with("a4q01"),
            .fns = ~ !is.na(.x)
        )
    ) %>%
    dplyr::mutate(
        num_crops_on_plot = rowSums(
            dplyr::select(., tidyselect::starts_with("a4q01")),
            na.rm = TRUE
        ),
        has_crop = num_crops_on_plot >= 1
    )

attrib_num_plots_w_crops <- plots_w_indicators %>%
    susoreview::count_obs(
        where = has_crop == 1,
        attrib_name = "num_plots_w_crops",
        attrib_vars = "a4q01"
    )

# whether plot used for crops, but has no crops
attrib_crop_use_plot_no_crop <- plots_w_indicators %>%
    susoreview::any_obs(
        where = (a3q05 == 1 | a3q05 == 2) & (has_crop == 0),
        attrib_name = "crop_use_plot_no_crop",
        attrib_vars = "a3q05|a4q01"
    )

# purestand plot, but has more than one crop planted
attrib_purestand_plot_multiple_crops <- plots_w_indicators %>%
    susoreview::any_obs(
        where = (a3q19 == 1) & 	(num_crops_on_plot > 1),
        attrib_name = "purestand_plot_multiple_crops",
        attrib_vars = "a3q19|a4q01"
    )

# mixed stand plot, but only one crop planted
attrib_mixstand_plot_multiple_crops <- plots_w_indicators %>%
    susoreview::any_obs(
        where = (a3q19 == 2) & 	(num_crops_on_plot == 1),
        attrib_name = "mixstand_plot_multiple_crops",
        attrib_vars = "a3q19|a4q01"
    )

# estimated area zero or missing
attrib_farmer_plot_area_missing <- plots_w_indicators %>%
    susoreview::any_obs(
        where = (a3q06a %in% c(0, -999999999)) | (haven::is_tagged_na(a3q06a)),
        attrib_name = "farmer_plot_area_missing",
        attrib_vars = "a3q06a"
    )

# measured area zero or missing
attrib_gps_plot_area_missing <- plots_w_indicators %>%
    susoreview::any_obs(
        where = (a3q06b %in% c(0, -999999999)) | (haven::is_tagged_na(a3q06b)),
        attrib_name = "gps_plot_area_missing",
        attrib_vars = "a3q06b"
    )

# mismatch between cropping pattern and number of crops reported
attrib_missing_plot_crop <- plots_w_indicators %>%
    susoreview::any_obs(
        where = a3q19 == 2 & num_crops_on_plot <= 1,
        attrib_name = "missing_plot_crop",
        attrib_vars = "a3q19|a4q01"
    )

attrib_extra_plot_crop <- plots_w_indicators %>%
    susoreview::any_obs(
        where = a3q19 == 1 & num_crops_on_plot > 1,
        attrib_name = "extra_plot_crop",
        attrib_vars = "a3q19|a4q01"
    )

# -----------------------------------------------------------------------------
# Parcel-plots
# -----------------------------------------------------------------------------

parcel_plots <- parcels %>%
    dplyr::inner_join(plots, by = c("interview__id", "interview__key", "HH_PARCELS__id")) %>%
    dplyr::mutate(
        parcel_cropped = hp2q21_1__2 >= 1,
        parcel_fallow = hp2q21_1__4 >= 1,
        plot_cropped = a3q05 %in% c(1, 2),
        plot_fallow = a3q05 == 3
    ) %>%
    dplyr::group_by(interview__id, interview__key, HH_PARCELS__id) %>%
    dplyr::summarise(
        dplyr::across(
            .cols = c(
                parcel_cropped, parcel_fallow, 
                plot_cropped, plot_fallow
            ),
            .fns = ~ any(.x == 1, na.rm = TRUE)
        )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
        parcel_cropped_plots_not = (parcel_cropped == 1 & plot_cropped == 0),
        parcel_fallow_plots_not = (parcel_fallow == 1 & plot_fallow == 0)
    )

# parcel marked as cropped, but no plot is cropped
attrib_parcel_cropped_plots_not <- parcel_plots %>%
    susoreview::any_obs(
        where = parcel_cropped_plots_not == 1,
        attrib_name = "parcel_cropped_plots_not",
        attrib_vars = "hp2q21_1|a3q05"
    )

# parcel marked as fallow, but no plot is fallow
attrib_parcel_fallow_plots_not <- parcel_plots %>%
    susoreview::any_obs(
        where = parcel_fallow_plots_not == 1,
        attrib_name = "parcel_fallow_plots_not",
        attrib_vars = "hp2q21_1|a3q05"
    )

# =============================================================================
# Combine attributes
# =============================================================================

# combine all attribute data sets whose names match the pattern below
attribs <- dplyr::bind_rows(mget(ls(pattern = "^attrib_")))

# remove intermediary objects to lighten load on memory
rm(list = ls(pattern = "^attrib_"))
