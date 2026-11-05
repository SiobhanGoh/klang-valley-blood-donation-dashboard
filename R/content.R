# Blood 101: Malaysia — general information, paraphrased from public pages of
# Pusat Darah Negara (National Blood Centre, Ministry of Health Malaysia), and
# the red-cell compatibility rule used by the compatibility explorer.
# Accessed 2 November 2026. Requirements can change: the dashboard always
# links to the source pages and tells readers to confirm with PDN.

PDN_SOURCES <- list(
  booklet = list(
    title = "Buku Informasi Pendermaan Darah (Pusat Darah Negara)",
    url = "https://pdn.gov.my/v2/images/dokumen/Buku_Informasi_Pendermaan_Darah.pdf"),
  az = list(
    title = "Donor eligibility A–Z (Pusat Darah Negara, updated 4 Feb 2026)",
    url = "https://pdn.gov.my/v2/index.php/informasi/bahan-pendidikan-2"),
  centres = list(
    title = "Pusat Pendermaan Darah Statik (Pusat Darah Negara)",
    url = "https://pdn.gov.my/v2/index.php/hubungi-kami/pusat-pendermaan-darah-statik")
)

COMPAT_SOURCES <- list(
  nhsbt = list(title = "What's your type? (NHS Blood and Transplant)",
               url = "https://www.blood.co.uk/why-give-blood/blood-types/whats-your-type/"),
  hemaq = list(title = "Blood types (Héma-Québec)",
               url = "https://www.hemaquebec.ca/en/learn/blood/blood-types")
)

ELIGIBILITY <- list(
  list(icon = "cake-candles", title = "Age",
       value = "17 to 70",
       detail = paste("First-time donors can be 17 to 60. Regular donors may continue up to 70;",
                      "those aged 61 to 70 need a yearly health screening. At 17, written",
                      "consent from a parent or guardian is needed."),
       short = "First time 17 to 60. At 61 to 70, a yearly health check. At 17, a parent's consent.",
       map = "Age", source = "booklet"),
  list(icon = "weight-scale", title = "Body weight",
       value = "45 kg or more",
       detail = paste("Donors of 45 to 55 kg give 350 ml; donors above 55 kg give 450 ml."),
       short = "45 to 55 kg give 350 ml. Above 55 kg give 450 ml.",
       map = "Weight", source = "booklet"),
  list(icon = "calendar-days", title = "Time between donations",
       value = "At least 3 months",
       detail = "For whole blood, the last donation must be at least three months earlier.",
       short = "Since your last whole-blood donation.",
       map = "Since last time", source = "booklet"),
  list(icon = "bed", title = "Sleep and food",
       value = "5 h sleep, a meal within 4 h",
       detail = paste("Sleep at least five hours the night before and eat within the four",
                      "hours before donating."),
       short = "Sleep the night before; eat in the 4 hours before.",
       map = "Sleep & food", source = "booklet"),
  list(icon = "droplet", title = "Haemoglobin check",
       value = "Checked on the day",
       detail = paste("A quick test before donating: above 12.5 g/dL for women and",
                      "13.5 g/dL for men."),
       short = "Above 12.5 g/dL for women, 13.5 g/dL for men.",
       map = "Haemoglobin", source = "booklet")
)

DEFERRAL_EXAMPLES <- list(
  list(what = "After antibiotics", wait = "2 weeks after the last dose", icon = "pills", tag = "2 wk", months = 0.5),
  list(what = "After acupuncture", wait = "3 months", icon = "syringe", tag = "3 mo", months = 3),
  list(what = "After minor surgery", wait = "3 months", icon = "bandage", tag = "3 mo", months = 3),
  list(what = "After major surgery", wait = "6 months", icon = "hospital", tag = "6 mo", months = 6),
  list(what = "After dengue", wait = "6 months after full recovery", icon = "mosquito", tag = "6 mo", months = 6)
)

# Getting ready, from PDN's donor booklet (Buku Informasi Pendermaan Darah).
PREP_TIPS <- list(
  list(icon = "bed", when = "The night before", title = "Sleep 5 hours or more",
       text = NULL),
  list(icon = "glass-water", when = "Before and on the day", title = "Drink enough water",
       text = "Keep drinking on the day itself."),
  list(icon = "utensils", when = "On the day", title = "Eat within 4 hours",
       text = "A proper meal in the four hours before you donate."),
  list(icon = "id-card", when = "Bring", title = "Your IC, driving licence or passport",
       text = "Repeat donors can also show their donor book with a photo."),
  list(icon = "shirt", when = "Wear", title = "Comfortable clothes",
       text = "If you wear long sleeves, make sure they roll up.")
)
# Iron sources PDN lists in the booklet (its section on anaemia).
IRON_FOODS <- list(
  list(icon = "leaf", name = "Spinach"), list(icon = "drumstick-bite", name = "Red meat"),
  list(icon = "fish", name = "Seafood"), list(icon = "seedling", name = "Beans and lentils")
)

DONATION_STEPS <- c(
  "Fill in the donation form.",
  "A quick health check: weight, blood group and haemoglobin.",
  "Register for your donation.",
  "A short interview with a doctor or health staff member to confirm you can donate.",
  "Donate blood.",
  "Have the food and drink provided."
)

# Short titles for the diagrams; the full sentences above and below are the content.
DONATION_STEP_TITLES <- c("Fill in the form", "Health check", "Register", "Short interview",
                          "Donate", "Food and drink")
DONATION_STEP_NOTES <- c(NA, "Weight, blood group and haemoglobin", NA,
                         "With a doctor or health staff member, to confirm you can donate", NA, NA)
AFTERCARE_TITLES <- c("Refreshments", "Extra water", "No alcohol", "Stay out of the sun",
                      "Take it easy", "Feeling dizzy?")

AFTERCARE_ART <- c("cup", "water", "noalcohol", "sun", "rest", "dizzy")

AFTERCARE <- c(
  "Eat and drink the refreshments provided before you leave.",
  "Drink extra water for the rest of the day.",
  "Avoid alcohol for 24 hours.",
  "Limit long periods in the sun to avoid dehydration.",
  "Avoid heavy lifting or strenuous activity for 6 to 8 hours.",
  "If you feel dizzy or faint, stop, then sit or lie down until it passes."
)

# Static (fixed) donation centres in the Klang Valley listed by PDN.
DONATION_CENTRES <- list(
  # wk / we: opening hours on weekdays and at weekends, as decimal hours (7.5 = 7.30 am).
  list(name = "Pusat Darah Negara", area = "Jalan Tun Razak, Kuala Lumpur",
       hours = "Mon–Fri 7.30 am–5.30 pm; weekends and public holidays 8 am–4 pm",
       wk = c(7.5, 17.5), we = c(8, 16)),
  list(name = "PDN Donation Suite, Mid Valley Megamall", area = "Kuala Lumpur",
       hours = "Daily 10 am–9 pm", wk = c(10, 21), we = c(10, 21)),
  list(name = "St John Ambulance Blood Donation Suite, 1 Utama", area = "Petaling Jaya, Selangor",
       hours = "Daily 11 am–7 pm", wk = c(11, 19), we = c(11, 19)),
  list(name = "St John Ambulance Blood Donation Suite, IOI City Mall", area = "Putrajaya",
       hours = "Mon–Fri 11 am–4 pm; weekends and public holidays 11 am–6 pm",
       wk = c(11, 16), we = c(11, 18))
)

# --- Red-cell compatibility ---------------------------------------------------
# A recipient can receive red cells from a donor whose A/B antigens they also
# carry, and an RhD-negative recipient needs RhD-negative red cells. This is a
# simplified educational rule: real transfusions are matched and cross-matched
# in a laboratory and decided by clinicians.

BLOOD_TYPES <- c("O-", "O+", "A-", "A+", "B-", "B+", "AB-", "AB+")

abo_antigens <- function(type) {
  abo <- sub("[+-]$", "", type)
  switch(abo, O = character(), A = "A", B = "B", AB = c("A", "B"))
}

can_give_red_cells <- function(donor, recipient) {
  abo_ok <- all(abo_antigens(donor) %in% abo_antigens(recipient))
  rh_ok <- endsWith(donor, "-") || endsWith(recipient, "+")
  abo_ok && rh_ok
}

compatibility_for <- function(type) {
  stopifnot(type %in% BLOOD_TYPES)
  list(
    gives_to = BLOOD_TYPES[vapply(BLOOD_TYPES, function(r) can_give_red_cells(type, r), logical(1))],
    receives_from = BLOOD_TYPES[vapply(BLOOD_TYPES, function(d) can_give_red_cells(d, type), logical(1))]
  )
}

# --- Plain-language glossary ---------------------------------------------------
# Shown when a reader hovers or taps a dotted term, and listed in the
# Sources & notes drawer.
GLOSSARY <- list(
  forecast = c("Forecast", "A method's estimate, made in advance, of how many donations will be recorded on a day."),
  error = c("Average error (MAE)", "How far off a forecast was, on average: the gap between the forecast and the real count, in donations per blood group per day. Smaller is better."),
  rmse = c("Big-miss error (RMSE)", "Like the average error, but large misses count extra. If it is much bigger than the average error, a method sometimes misses badly."),
  bias = c("Average bias", "The real count minus the forecast, averaged. Positive means forecasts were too low on average; negative means too high. Near zero can still hide big misses that cancel out."),
  test_period = c("Test period", "The year a method is tested on. It is predicted using only earlier years, so the test is fair. The current year counts once it has four weeks of data."),
  pooled = c("All groups together", "Errors from the four blood groups A, AB, B and O combined into one score."),
  lag = c("Seven-day baseline", "The simplest forecast: tomorrow will be like the same day last week. It is the yardstick the other methods have to beat."),
  weekday_mean = c("Weekday average", "The average number of donations on that weekday (Mondays, Tuesdays...) in past years."),
  regression = c("Linear regression", "A model that learns the usual weekday pattern plus a steady long-term trend from past years."),
  low_day = c("Low-collection day", "A day at or below the lower quartile from earlier years: one of the quietest quarter of days for that blood group. It is not a clinical shortage level."),
  quartile = c("Lower quartile", "The value with a quarter of past days below it. A quarter of days are this quiet or quieter."),
  overpredict = c("Forecast too high", "When the forecast was more than the real count. On quiet days the methods tend to expect more donations than arrive."),
  coefficient = c("Coefficient", "A number the regression learned, such as how many more donations a Sunday brings than a Monday."),
  r2 = c("Fit (R²)", "How much of the day-to-day ups and downs the regression explains, from 0 (none) to 1 (all)."),
  retrospective = c("Tested on the past", "The methods are scored on days that have already happened, not on live forecasts acted on in real time."),
  abo = c("ABO group", "The letter part of a blood type: A, B, AB or O. The donation data records only this."),
  rhd = c("RhD", "The plus or minus part of a blood type. RhD-negative people need RhD-negative red cells."),
  red_cells = c("Red cells", "The part of blood that carries oxygen; the compatibility rules here are for red-cell transfusions.")
)
