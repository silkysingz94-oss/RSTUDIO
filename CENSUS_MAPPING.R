install.packages(c(
  "officer",
  "flextable",
  "rvg"
))
# ================================================================
# GAMBIA LABOUR FORCE SURVEY 2026
# LISTING EXERCISE TRAINING
# PowerPoint Presentation Generator
#
# Generated using R + officer
# ================================================================

# -----------------------------
# 1. LOAD PACKAGES
# -----------------------------

library(officer)
library(flextable)
library(rvg)

# -----------------------------
# 2. OUTPUT FILE
# -----------------------------

output_file <- "Gambia_LFS_2026_Listing_Exercise_Training.pptx"

# -----------------------------
# 3. PRESENTATION SETTINGS
# -----------------------------

ppt <- read_pptx()

# Standard widescreen layout
ppt <- layout_properties(ppt)

# Use widescreen PowerPoint
ppt <- read_pptx()

# -----------------------------
# 4. COLOUR PALETTE
# -----------------------------

NAVY       <- "#12355B"
BLUE       <- "#1F5A94"
LIGHT_BLUE <- "#DCEAF7"
GOLD       <- "#D4A72C"
GREEN      <- "#2E7D32"
RED        <- "#B3261E"
ORANGE     <- "#E67E22"
GREY       <- "#5F6368"
LIGHT_GREY <- "#F3F5F7"
WHITE      <- "#FFFFFF"
BLACK      <- "#1F1F1F"

# -----------------------------
# 5. HELPER FUNCTIONS
# -----------------------------

add_title <- function(ppt, title, subtitle = NULL) {
  
  ppt <- add_text(
    ppt,
    title,
    x = 0.55,
    y = 0.25,
    w = 12.2,
    h = 0.65,
    font_size = 26,
    bold = TRUE,
    color = NAVY
  )
  
  if (!is.null(subtitle)) {
    ppt <- add_text(
      ppt,
      subtitle,
      x = 0.58,
      y = 0.90,
      w = 11.8,
      h = 0.40,
      font_size = 12,
      color = GREY
    )
  }
  
  ppt
}


add_text <- function(
    ppt,
    text,
    x,
    y,
    w,
    h,
    font_size = 18,
    bold = FALSE,
    color = BLACK,
    align = "left"
) {
  
  fp <- fp_text(
    font.size = font_size,
    bold = bold,
    color = color,
    font.family = "Aptos"
  )
  
  ppt <- ph_with(
    ppt,
    value = fpar(
      ftext(text, prop = fp)
    ),
    location = ph_location(
      left = x,
      top = y,
      width = w,
      height = h
    )
  )
  
  ppt
}


add_bullets <- function(
    ppt,
    bullets,
    x = 0.75,
    y = 1.35,
    w = 11.7,
    h = 5.6,
    font_size = 18,
    color = BLACK
) {
  
  runs <- lapply(
    bullets,
    function(x) {
      ftext(
        paste0("• ", x),
        prop = fp_text(
          font.size = font_size,
          color = color,
          font.family = "Aptos"
        )
      )
    }
  )
  
  paras <- lapply(
    runs,
    function(r) {
      fpar(r)
    }
  )
  
  ppt <- ph_with(
    ppt,
    value = block_list(paras),
    location = ph_location(
      left = x,
      top = y,
      width = w,
      height = h
    )
  )
  
  ppt
}


add_section <- function(
    ppt,
    section_number,
    title,
    subtitle
) {
  
  ppt <- add_slide(
    ppt,
    layout = "Blank",
    master = "Office Theme"
  )
  
  # Background
  ppt <- ph_with(
    ppt,
    value = "",
    location = ph_location_fullsize()
  )
  
  ppt <- add_text(
    ppt,
    paste0("SECTION ", section_number),
    x = 0.8,
    y = 2.0,
    w = 11.5,
    h = 0.6,
    font_size = 18,
    bold = TRUE,
    color = GOLD
  )
  
  ppt <- add_text(
    ppt,
    title,
    x = 0.8,
    y = 2.55,
    w = 11.5,
    h = 1.2,
    font_size = 32,
    bold = TRUE,
    color = NAVY
  )
  
  ppt <- add_text(
    ppt,
    subtitle,
    x = 0.85,
    y = 3.75,
    w = 10.8,
    h = 1.0,
    font_size = 18,
    color = GREY
  )
  
  ppt
}


add_footer <- function(ppt, text = "Gambia Labour Force Survey 2026 | Listing Exercise Training") {
  
  ppt <- ph_with(
    ppt,
    value = text,
    location = ph_location(
      left = 0.55,
      top = 7.05,
      width = 11.8,
      height = 0.25
    )
  )
  
  ppt
}


add_standard_slide <- function(
    ppt,
    title,
    bullets,
    subtitle = NULL,
    font_size = 18
) {
  
  ppt <- add_slide(
    ppt,
    layout = "Blank",
    master = "Office Theme"
  )
  
  ppt <- add_title(
    ppt,
    title,
    subtitle
  )
  
  ppt <- add_bullets(
    ppt,
    bullets,
    font_size = font_size
  )
  
  ppt <- add_footer(ppt)
  
  ppt
}


# ================================================================
# SLIDE 1 – TITLE
# ================================================================

ppt <- add_slide(
  ppt,
  layout = "Blank",
  master = "Office Theme"
)

ppt <- add_text(
  ppt,
  "GAMBIA LABOUR FORCE SURVEY 2026",
  x = 0.8,
  y = 1.55,
  w = 11.5,
  h = 0.8,
  font_size = 30,
  bold = TRUE,
  color = NAVY
)

ppt <- add_text(
  ppt,
  "LISTING EXERCISE TRAINING",
  x = 0.8,
  y = 2.45,
  w = 11.5,
  h = 0.8,
  font_size = 38,
  bold = TRUE,
  color = BLUE
)

ppt <- add_text(
  ppt,
  "Training Programme, Procedures, Field Protocols and Quality Assurance",
  x = 0.85,
  y = 3.45,
  w = 10.8,
  h = 0.7,
  font_size = 20,
  color = GREY
)

ppt <- add_text(
  ppt,
  "Gambia Bureau of Statistics",
  x = 0.85,
  y = 5.15,
  w = 6,
  h = 0.5,
  font_size = 20,
  bold = TRUE,
  color = NAVY
)

ppt <- add_text(
  ppt,
  "2026",
  x = 0.85,
  y = 5.65,
  w = 3,
  h = 0.5,
  font_size = 18,
  color = GREY
)

# ================================================================
# SLIDE 2 – TRAINING OVERVIEW
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Training Overview",
  c(
    "Introduction to the Labour Force Survey and listing exercise",
    "Lessons learned and errors from previous listing exercises",
    "Enumeration Area concepts and boundary identification",
    "Household and structure identification",
    "Map reading and GPS procedures",
    "Tablet and electronic listing application",
    "Roles and responsibilities of field personnel",
    "Communication, supervision and quality control",
    "Practical mock listing exercise and assessment",
    "Final deployment briefing"
  )
)

# ================================================================
# SLIDE 3 – WHY THE LISTING EXERCISE MATTERS
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Why the Listing Exercise Matters",
  c(
    "The listing exercise provides the sampling frame for the Labour Force Survey.",
    "A complete and accurate listing improves the quality of sample selection.",
    "Errors during listing can result in household omission, duplication or incorrect selection.",
    "Accurate EA identification is essential for ensuring geographic coverage.",
    "Reliable GPS information supports spatial verification and monitoring.",
    "The quality of the listing directly affects the quality of the main survey."
  )
)

# ================================================================
# SLIDE 4 – TRAINING OBJECTIVES
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Training Objectives",
  c(
    "Understand the purpose and importance of the LFS listing exercise.",
    "Apply correct household listing concepts and definitions.",
    "Identify and remain within assigned Enumeration Areas.",
    "Read and interpret printed and digital EA maps.",
    "Use tablets, GPS and the electronic listing application correctly.",
    "Understand individual roles and responsibilities.",
    "Apply field supervision and quality-control procedures.",
    "Communicate and report field problems through the correct channels.",
    "Demonstrate readiness before deployment."
  )
)

# ================================================================
# SECTION 1
# ================================================================

ppt <- add_section(
  ppt,
  "01",
  "Lessons from Previous Listing Exercises",
  "Understanding common errors and preventing their recurrence."
)

# ================================================================
# SLIDE 6
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Common Errors from Previous Listing Exercises",
  c(
    "Incorrect identification of Enumeration Areas.",
    "Failure to follow EA boundaries.",
    "Listing households outside the assigned EA.",
    "Omission of households or residential structures.",
    "Duplicate listing of households.",
    "Incorrect household numbering.",
    "Incomplete or incorrect household information.",
    "Confusion between households, structures, housing units and compounds.",
    "Incorrect treatment of vacant or abandoned structures."
  )
)

# ================================================================
# SLIDE 7
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "GPS and Digital Data Collection Errors",
  c(
    "GPS points captured at the wrong location.",
    "Multiple households recorded at the same GPS location without verification.",
    "Poor GPS accuracy not properly investigated.",
    "Failure to synchronize completed records.",
    "Missing or incomplete digital records.",
    "Incorrect use of the listing application.",
    "Failure to report application problems.",
    "Insufficient checking of records before submission."
  )
)

# ================================================================
# SLIDE 8
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Lessons Learned",
  c(
    "Always confirm the EA before starting work.",
    "Use both the map and field landmarks for orientation.",
    "Follow a systematic movement pattern.",
    "Do not rely on assumptions when identifying households.",
    "Verify unusual or suspicious records.",
    "Check GPS points before leaving the location.",
    "Synchronize data regularly.",
    "Report problems immediately to the supervisor.",
    "Supervisors must verify completed EAs before acceptance."
  )
)

# ================================================================
# SECTION 2
# ================================================================

ppt <- add_section(
  ppt,
  "02",
  "Introduction to the Labour Force Survey",
  "Survey purpose, concepts and the relationship between listing and the main survey."
)

# ================================================================
# SLIDE 10
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "What is the Labour Force Survey?",
  c(
    "The Labour Force Survey provides information on the labour market and economic activity of the population.",
    "It provides evidence on employment, unemployment and labour force participation.",
    "Labour force statistics support national planning and policy development.",
    "The survey requires a reliable and up-to-date sampling frame.",
    "The listing exercise is therefore a critical preparation stage."
  )
)

# ================================================================
# SLIDE 11
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Key Labour Force Concepts",
  c(
    "Employment – persons engaged in work according to the survey definition.",
    "Unemployment – persons without employment who meet the applicable criteria.",
    "Labour force – persons classified as employed or unemployed.",
    "Persons outside the labour force – persons not classified as part of the labour force.",
    "Definitions must be applied consistently during the main survey."
  )
)

# ================================================================
# SECTION 3
# ================================================================

ppt <- add_section(
  ppt,
  "03",
  "Enumeration Areas and Boundaries",
  "Correct geographic identification is the foundation of complete listing."
)

# ================================================================
# SLIDE 13
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Understanding the Enumeration Area",
  c(
    "An Enumeration Area (EA) is a defined geographic area assigned for field enumeration.",
    "Each EA has a unique identification code.",
    "Listers must work only within their assigned EA.",
    "The EA map provides the geographic reference for the assignment.",
    "All structures and eligible households within the EA must be considered."
  )
)

# ================================================================
# SLIDE 14
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "EA Boundary Identification",
  c(
    "Review the EA map before starting fieldwork.",
    "Identify roads, rivers, settlements and other landmarks.",
    "Confirm the starting point and movement direction.",
    "Use physical features to help interpret boundaries.",
    "Do not assume that a road or settlement automatically defines an EA boundary.",
    "Report unclear or conflicting boundaries to the supervisor.",
    "Do not independently redefine official geographical boundaries."
  )
)

# ================================================================
# SLIDE 15
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "When the Map and Ground Situation Do Not Match",
  c(
    "Stop and assess the situation carefully.",
    "Check the printed and digital EA map.",
    "Confirm the EA code and surrounding landmarks.",
    "Inform the supervisor of the discrepancy.",
    "Record the location and nature of the problem.",
    "Escalate unresolved mapping issues through the GIS/Cartography team.",
    "Do not create, modify or redefine official boundaries in the field."
  )
)

# ================================================================
# SECTION 4
# ================================================================

ppt <- add_section(
  ppt,
  "04",
  "Household and Structure Identification",
  "Correct identification prevents omissions, duplication and incorrect listing."
)

# ================================================================
# SLIDE 17
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "What Must Be Distinguished?",
  c(
    "Household",
    "Housing unit",
    "Residential structure",
    "Compound",
    "Vacant structure",
    "Abandoned structure",
    "Under-construction structure",
    "Non-residential structure",
    "Institutional or collective living arrangement"
  )
)

# ================================================================
# SLIDE 18
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Complex Household Situations",
  c(
    "Multiple households may live within one structure.",
    "A household may occupy more than one structure.",
    "Several structures may exist within one compound.",
    "Vacant structures must be treated according to approved listing rules.",
    "Abandoned structures should not automatically be treated as occupied households.",
    "Institutions and collective living arrangements require special attention.",
    "When uncertain, consult the supervisor rather than making assumptions."
  )
)

# ================================================================
# SECTION 5
# ================================================================

ppt <- add_section(
  ppt,
  "05",
  "Household Listing Procedures",
  "A systematic approach ensures complete coverage and prevents duplication."
)

# ================================================================
# SLIDE 20
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Standard Listing Procedure",
  c(
    "1. Locate the assigned Enumeration Area.",
    "2. Confirm the EA code and boundaries.",
    "3. Identify the appropriate starting point.",
    "4. Review the map and surrounding landmarks.",
    "5. Move systematically through the EA.",
    "6. Identify eligible residential structures and households.",
    "7. Assign the appropriate listing or household number.",
    "8. Collect the required listing information.",
    "9. Capture the correct GPS location.",
    "10. Review the record before moving on."
  ),
  font_size = 16
)

# ================================================================
# SLIDE 21
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Completing the Listing Assignment",
  c(
    "Save and synchronize completed records.",
    "Review records for missing or inconsistent information.",
    "Report unusual cases to the supervisor.",
    "Revisit households where required.",
    "Verify that the entire EA has been covered.",
    "Check that households have not been omitted or duplicated.",
    "Do not declare an EA complete until the supervisor has verified the work."
  )
)

# ================================================================
# SECTION 6
# ================================================================

ppt <- add_section(
  ppt,
  "06",
  "Map Reading and GPS",
  "Using spatial information correctly in the field."
)

# ================================================================
# SLIDE 23
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Map Reading Skills",
  c(
    "Orient the map correctly.",
    "Identify the assigned EA.",
    "Locate the starting point.",
    "Recognize roads, settlements and landmarks.",
    "Use map features to maintain direction.",
    "Track progress through the EA.",
    "Compare field conditions with map information.",
    "Report mapping discrepancies."
  )
)

# ================================================================
# SLIDE 24
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "GPS Procedures",
  c(
    "Ensure the device has adequate GPS signal.",
    "Capture the GPS position at the correct household or structure location.",
    "Check GPS accuracy before accepting the point.",
    "Avoid capturing points from an unrelated location.",
    "Investigate stacked or repeated GPS points.",
    "Do not move away from the household before confirming the record.",
    "Report persistent GPS problems to the supervisor."
  )
)

# ================================================================
# SLIDE 25
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "GPS: What to Avoid",
  c(
    "Do not capture all households from the vehicle.",
    "Do not reuse a previous household's GPS location.",
    "Do not assume that a repeated coordinate is correct.",
    "Do not ignore poor accuracy.",
    "Do not record GPS points outside the assigned EA.",
    "Do not leave suspicious GPS records unchecked."
  )
)

# ================================================================
# SECTION 7
# ================================================================

ppt <- add_section(
  ppt,
  "07",
  "Tablet and Electronic Listing Application",
  "Accurate digital data collection and synchronization."
)

# ================================================================
# SLIDE 27
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Tablet Training",
  c(
    "Log in to the device and listing application.",
    "Select the correct EA.",
    "Navigate through the listing form.",
    "Enter household information correctly.",
    "Complete mandatory fields.",
    "Save and edit records.",
    "Review completed records.",
    "Capture GPS information.",
    "Synchronize data."
  )
)

# ================================================================
# SLIDE 28
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Device Management and Troubleshooting",
  c(
    "Keep tablets adequately charged.",
    "Carry power banks where provided.",
    "Protect devices from water, dust and physical damage.",
    "Do not share passwords or user credentials.",
    "Report application errors to the appropriate technical support.",
    "Do not delete records without authorization.",
    "Synchronize data regularly.",
    "Maintain confidentiality and data security."
  )
)

# ================================================================
# SECTION 8
# ================================================================

ppt <- add_section(
  ppt,
  "08",
  "Roles and Responsibilities",
  "Everyone has a defined role in ensuring successful fieldwork."
)

# ================================================================
# SLIDE 30
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Field Coordinator",
  c(
    "Coordinate overall field deployment.",
    "Monitor progress across field teams.",
    "Maintain communication with headquarters.",
    "Resolve major operational issues.",
    "Coordinate logistics.",
    "Review field reports.",
    "Ensure implementation follows approved procedures."
  )
)

# ================================================================
# SLIDE 31
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Supervisors",
  c(
    "Confirm assignments before deployment.",
    "Review EA maps with listers.",
    "Monitor daily field progress.",
    "Conduct spot checks.",
    "Review submitted records.",
    "Check for omissions, duplicates and inconsistencies.",
    "Verify GPS locations where necessary.",
    "Ensure listers remain within assigned EAs.",
    "Resolve field problems.",
    "Report unresolved issues.",
    "Verify completed EAs before acceptance."
  ),
  font_size = 16
)

# ================================================================
# SLIDE 32
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Listers / Enumerators",
  c(
    "Work only within the assigned EA.",
    "Correctly interpret the EA map.",
    "Identify all eligible households.",
    "Follow approved listing procedures.",
    "Record information accurately.",
    "Avoid assumptions or fabrication.",
    "Capture accurate GPS locations.",
    "Review records before submission.",
    "Synchronize completed records.",
    "Maintain confidentiality.",
    "Report problems immediately."
  ),
  font_size = 16
)

# ================================================================
# SLIDE 33
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Drivers",
  c(
    "Transport field teams safely.",
    "Follow the approved movement plan.",
    "Report to meeting points on time.",
    "Maintain vehicle readiness.",
    "Report vehicle problems immediately.",
    "Support communication when necessary.",
    "Transport and collect field teams from agreed locations.",
    "Avoid unauthorized or unnecessary movements.",
    "Do not interfere with survey data collection."
  )
)

# ================================================================
# SLIDE 34
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "GIS / Cartography Team",
  c(
    "Prepare and provide EA maps.",
    "Support map interpretation.",
    "Address mapping and EA boundary problems.",
    "Provide technical clarification on geographic issues.",
    "Support GPS and spatial data quality checks.",
    "Assist supervisors with unresolved mapping issues.",
    "Maintain consistency between approved geographic information and field operations."
  )
)

# ================================================================
# SLIDE 35
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "IT / Data Management Team",
  c(
    "Manage user accounts.",
    "Provide technical support.",
    "Support data synchronization.",
    "Monitor data transmission.",
    "Troubleshoot application problems.",
    "Support data quality monitoring.",
    "Maintain data security and backup procedures."
  )
)

# ================================================================
# SECTION 9
# ================================================================

ppt <- add_section(
  ppt,
  "09",
  "Communication, Reporting and Quality Control",
  "Problems must be reported early and quality must be checked continuously."
)

# ================================================================
# SLIDE 37
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Field Communication",
  c(
    "Know the reporting structure before deployment.",
    "Maintain daily communication with supervisors.",
    "Report missing or incorrect EA maps.",
    "Report inaccessible areas.",
    "Report GPS and tablet problems.",
    "Report security and logistical challenges.",
    "Use established escalation procedures.",
    "Do not make unauthorized decisions."
  )
)

# ================================================================
# SLIDE 38
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Quality Assurance and Quality Control",
  c(
    "Review completed records daily.",
    "Check for missing information.",
    "Identify duplicate records.",
    "Verify suspicious or unusual records.",
    "Check GPS locations.",
    "Conduct random spot checks.",
    "Verify complete EA coverage.",
    "Ensure households have not been omitted.",
    "Require corrections or revisits where necessary.",
    "Monitor lister productivity and performance."
  ),
  font_size = 16
)

# ================================================================
# SLIDE 39
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Quality Control Principle",
  c(
    "QUALITY CONTROL IS CONTINUOUS.",
    "Do not wait until the end of the exercise to identify errors.",
    "Supervisors should review records every day.",
    "Problems should be corrected while teams are still in or near the EA.",
    "An EA should only be accepted after adequate verification."
  ),
  font_size = 20
)

# ================================================================
# SECTION 10
# ================================================================

ppt <- add_section(
  ppt,
  "10",
  "Practical Field Exercise",
  "Training should simulate real field conditions as closely as possible."
)

# ================================================================
# SLIDE 41
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Mock EA Exercise",
  c(
    "Read and interpret the EA map.",
    "Identify the EA boundary.",
    "Locate the starting point.",
    "Identify residential structures.",
    "Determine eligible households.",
    "Assign household numbers.",
    "Record household information.",
    "Capture GPS coordinates.",
    "Handle absent households.",
    "Handle multiple households in one compound.",
    "Handle vacant structures.",
    "Synchronize completed records.",
    "Conduct supervisory verification."
  ),
  font_size = 15
)

# ================================================================
# SLIDE 42
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Error Identification Exercise",
  c(
    "Participants will receive deliberately incorrect records and scenarios.",
    "Identify household omissions.",
    "Identify duplicate listings.",
    "Identify incorrect EA assignments.",
    "Identify incorrect household numbers.",
    "Identify incorrect GPS locations.",
    "Identify incomplete information.",
    "Identify incorrect treatment of vacant structures.",
    "Identify failure to revisit absent households.",
    "Identify listing outside the assigned EA."
  ),
  font_size = 16
)

# ================================================================
# SECTION 11
# ================================================================

ppt <- add_section(
  ppt,
  "11",
  "Professional Conduct and Field Ethics",
  "Respect, confidentiality and accuracy are mandatory."
)

# ================================================================
# SLIDE 44
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Field Ethics",
  c(
    "Treat respondents with respect.",
    "Introduce yourself and the survey properly.",
    "Maintain confidentiality.",
    "Remain neutral and professional.",
    "Never fabricate information.",
    "Do not disclose unauthorized information.",
    "Respect local communities and customs.",
    "Wear appropriate identification.",
    "Use survey equipment responsibly."
  )
)

# ================================================================
# SECTION 12
# ================================================================

ppt <- add_section(
  ppt,
  "12",
  "Daily Fieldwork Routine",
  "Prepare properly, work systematically and close the day correctly."
)

# ================================================================
# SLIDE 46
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Before Departure",
  c(
    "Check tablets and equipment.",
    "Check battery levels and power banks.",
    "Confirm EA assignments.",
    "Check printed and digital maps.",
    "Confirm transportation arrangements.",
    "Confirm communication channels.",
    "Review any outstanding issues from the previous day."
  )
)

# ================================================================
# SLIDE 47
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "During Fieldwork",
  c(
    "Stay within the assigned EA.",
    "Follow the planned movement pattern.",
    "List every eligible household.",
    "Capture accurate GPS information.",
    "Review each record before moving on.",
    "Communicate problems immediately.",
    "Follow supervisor instructions."
  )
)

# ================================================================
# SLIDE 48
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "End-of-Day Routine",
  c(
    Stay within the assigned EA.",
    "Follow the planned movement pattern.",
    "List every eligible household.",
    "Capture accurate GPS information.",
    "Review each record before moving on.",
    "Communicate problems immediately.",
    "Follow supervisor instructions."
  )
)

# ================================================================
# SECTION 13
# ================================================================

ppt <- add_section(
  ppt,
  "13",
  "Assessment and Deployment",
  "Every participant must demonstrate readiness before field deployment."
)

# ================================================================
# SLIDE 50
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Assessment Areas",
  c(
    "Labour Force Survey concepts.",
    "Household and structure definitions.",
    "Enumeration Area identification.",
    "Map reading.",
    "Listing procedures.",
    "GPS use.",
    "Tablet and application use.",
    "Field ethics.",
    "Roles and responsibilities.",
    "Quality-control procedures."
  )
)

# ================================================================
# SLIDE 51
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Practical Competency Assessment",
  c(
    "Identify the correct EA.",
    "Interpret the EA map.",
    "Locate the starting point.",
    "Identify households correctly.",
    "Apply household numbering.",
    "Capture accurate GPS coordinates.",
    "Enter and review digital records.",
    "Synchronize completed records.",
    "Identify listing errors.",
    "Explain the correct corrective procedure."
  )
)

# ================================================================
# SLIDE 52
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Final Deployment Briefing",
  c(
    "COVER THE ENTIRE ASSIGNED EA.",
    "DO NOT LIST OUTSIDE THE ASSIGNED EA.",
    "DO NOT OMIT OR DUPLICATE HOUSEHOLDS.",
    "RECORD INFORMATION ACCURATELY.",
    "CAPTURE CORRECT GPS COORDINATES.",
    "FOLLOW APPROVED PROCEDURES.",
    "DO NOT MAKE ASSUMPTIONS.",
    "REPORT PROBLEMS IMMEDIATELY.",
    "SUPERVISORS MUST VERIFY COMPLETED WORK."
  ),
  font_size = 19
)

# ================================================================
# SLIDE 53 – FIVE GOLDEN RULES
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "The Five Golden Rules of Listing",
  c(
    "COVER THE ENTIRE ASSIGNED EA.",
    "DO NOT LIST OUTSIDE THE ASSIGNED EA.",
    "DO NOT OMIT OR DUPLICATE HOUSEHOLDS.",
    "RECORD INFORMATION ACCURATELY.",
    "CAPTURE CORRECT GPS COORDINATES.",
    "FOLLOW APPROVED PROCEDURES.",
    "DO NOT MAKE ASSUMPTIONS.",
    "REPORT PROBLEMS IMMEDIATELY.",
    "SUPERVISORS MUST VERIFY COMPLETED WORK."
  ),
  ),
  font_size = 22
)

# ================================================================
# SLIDE 54 – PROPOSED TRAINING SCHEDULE
# ================================================================

ppt <- add_slide(
  ppt,
  layout = "Blank",
  master = "Office Theme"
)

ppt <- add_title(
  ppt,
  "Proposed Five-Day Training Schedule"
)

schedule <- data.frame(
  Day = c(
    "Day 1",
    "Day 2",
    "Day 3",
    "Day 4",
    "Day 5"
  ),
  `Main Training Areas` = c(
    "Opening, LFS introduction, objectives, previous listing errors, concepts and definitions",
    "Enumeration Areas, maps, boundaries, household identification and listing procedures",
    "Tablet/application training, GPS, data entry, synchronization and troubleshooting",
    "Roles and responsibilities, supervision, quality control, communication and reporting",
    "Practical mock listing, error identification, assessment, feedback and final deployment briefing"
  )
)

ft <- flextable(schedule)

ft <- theme_booktabs(ft)

ft <- autofit(ft)

ft <- fontsize(
  ft,
  size = 15,
  part = "all"
)

ft <- bold(
  ft,
  part = "header"
)

ft <- bg(
  ft,
  bg = NAVY,
  part = "header"
)

ft <- color(
  ft,
  color = WHITE,
  part = "header"
)

ppt <- ph_with(
  ppt,
  value = ft,
  location = ph_location(
    left = 0.65,
    top = 1.5,
    width = 11.8,
    height = 4.8
  )
)

ppt <- add_footer(ppt)

# ================================================================
# SLIDE 55 – EXPECTED OUTCOMES
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Expected Training Outcomes",
  c(
    "Explain the purpose and importance of the LFS listing exercise.",
    "Correctly identify and work within assigned Enumeration Areas.",
    "Interpret EA maps and geographical information.",
    "Correctly identify and list eligible households.",
    "Use tablets and the electronic listing application effectively.",
    "Capture accurate GPS coordinates.",
    "Identify and avoid common listing errors.",
    "Understand individual roles and responsibilities.",
    "Apply appropriate quality-control procedures.",
    "Report field problems through the correct channels.",
    "Demonstrate professional and ethical conduct.",
    "Successfully complete a practical listing exercise."
  ),
  font_size = 16
)

# ================================================================
# SLIDE 56 – KEY MESSAGE
# ================================================================

ppt <- add_slide(
  ppt,
  layout = "Blank",
  master = "Office Theme"
)

ppt <- add_text(
  ppt,
  "KEY MESSAGE",
  x = 0.8,
  y = 1.35,
  w = 11.2,
  h = 0.6,
  font_size = 22,
  bold = TRUE,
  color = GOLD
)

ppt <- add_text(
  ppt,
  "A COMPLETE EA + ACCURATE LISTING + CORRECT GPS + STRONG SUPERVISION",
  x = 0.8,
  y = 2.25,
  w = 11.5,
  h = 1.5,
  font_size = 30,
  bold = TRUE,
  color = NAVY
)

ppt <- add_text(
  ppt,
  "= A RELIABLE SAMPLING FRAME FOR THE LABOUR FORCE SURVEY",
  x = 0.8,
  y = 4.05,
  w = 11.5,
  h = 1.0,
  font_size = 24,
  bold = TRUE,
  color = BLUE
)

# ================================================================
# SLIDE 57 – CONCLUSION
# ================================================================

ppt <- add_standard_slide(
  ppt,
  "Conclusion",
  c(
    "The success of the Labour Force Survey depends significantly on the quality of the listing exercise.",
    "Training must combine theory, demonstrations, practical exercises and assessment.",
    "Particular attention must be given to EA identification and complete household coverage.",
    "GPS accuracy and correct use of digital tools are essential.",
    "Effective communication and supervision are critical to resolving field problems.",
    "Every assigned EA must be completely and accurately covered.",
    "The resulting listing data must be sufficiently reliable to support sample selection and subsequent fieldwork."
  ),
  font_size = 17
)

# ================================================================
# SLIDE 58 – THANK YOU
# ================================================================

ppt <- add_slide(
  ppt,
  layout = "Blank",
  master = "Office Theme"
)

ppt <- add_text(
  ppt,
  "THANK YOU",
  x = 0.8,
  y = 2.25,
  w = 11.5,
  h = 1.0,
  font_size = 42,
  bold = TRUE,
  color = NAVY
)

ppt <- add_text(
  ppt,
  "Questions, Discussion and Clarifications",
  x = 0.85,
  y = 3.35,
  w = 11,
  h = 0.7,
  font_size = 24,
  color = BLUE
)

ppt <- add_text(
  ppt,
  "Gambia Bureau of Statistics\nLabour Force Survey 2026",
  x = 0.85,
  y = 4.65,
  w = 8,
  h = 1.0,
  font_size = 18,
  color = GREY
)

# ================================================================
# SAVE POWERPOINT
# ================================================================

print(
  ppt,
  target = output_file
)

cat("\n====================================================\n")
cat("POWERPOINT CREATED SUCCESSFULLY\n")
cat("====================================================\n")
cat("File:", output_file, "\n")
cat("====================================================\n")

getwd()
