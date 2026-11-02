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
       source = "booklet"),
  list(icon = "weight-scale", title = "Body weight",
       value = "45 kg or more",
       detail = paste("Donors of 45 to 55 kg give 350 ml; donors above 55 kg give 450 ml."),
       source = "booklet"),
  list(icon = "calendar-days", title = "Time between donations",
       value = "At least 3 months",
       detail = "For whole blood, the last donation must be at least three months earlier.",
       source = "booklet"),
  list(icon = "bed", title = "Sleep and food",
       value = "5 h sleep, a meal within 4 h",
       detail = paste("Sleep at least five hours the night before and eat within the four",
                      "hours before donating."),
       source = "booklet"),
  list(icon = "droplet", title = "Haemoglobin check",
       value = "Checked on the day",
       detail = paste("A quick test before donating: above 12.5 g/dL for women and",
                      "13.5 g/dL for men."),
       source = "booklet")
)

DEFERRAL_EXAMPLES <- list(
  list(what = "After antibiotics", wait = "2 weeks after the last dose"),
  list(what = "After acupuncture", wait = "3 months"),
  list(what = "After minor surgery", wait = "3 months"),
  list(what = "After major surgery", wait = "6 months"),
  list(what = "After dengue", wait = "6 months after full recovery")
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
  list(name = "Pusat Darah Negara", area = "Jalan Tun Razak, Kuala Lumpur",
       hours = "Mon–Fri 7.30 am–5.30 pm; weekends and public holidays 8 am–4 pm"),
  list(name = "PDN Donation Suite, Mid Valley Megamall", area = "Kuala Lumpur",
       hours = "Daily 10 am–9 pm"),
  list(name = "St John Ambulance Blood Donation Suite, 1 Utama", area = "Petaling Jaya, Selangor",
       hours = "Daily 11 am–7 pm"),
  list(name = "St John Ambulance Blood Donation Suite, IOI City Mall", area = "Putrajaya",
       hours = "Mon–Fri 11 am–4 pm; weekends and public holidays 11 am–6 pm")
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
