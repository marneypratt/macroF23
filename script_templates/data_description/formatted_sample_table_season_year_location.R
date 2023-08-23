
#To get a formatted table, make sure that the {flextable} package is loaded
#Use additional code to create the summary table called "table.sum"
#If you want to format the table yourself, then don't use this code

#see https://davidgohel.github.io/flextable/ for more information and formatting options

#create the formatted table
ft <- flextable(table.sum)

#create header labels
ft <- set_header_labels(ft,
                        season = "Season",
                        year = "Year",
                        location = "Location",
                        dates = "Dates",
                        samples = "# of Samples",
                        area = "Total Area Sampled (m^2)"
                        )

#format superscript
ft <- compose( 
  ft, j = "area", part="header",
  value = as_paragraph(
    "Total Area Sampled (m", 
    as_sup("2"),
    ")"
  )
)

#bold the headings
ft <- bold(ft, part="header")

ft <- merge_v(ft, j = ~ season + year)

#set table properties
ft <- set_table_properties(ft, layout = "autofit", width = 1)

#add lines between year x season
ft <- theme_vanilla(ft) %>% 
  fontsize(size = 10)

#center columns
ft <- align(ft, align = "center", part = "all" )

ft <- fix_border_issues(ft)

#print the table
#right click on the table, choose select all, 
#choose copy, then paste in your document
#finish formatting as needed in your document
ft

