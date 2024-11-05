
# packages ----------------------------------------------------------------

library(tidyverse)
library(openxlsx)
library(patchwork)
library(Cairo)

# data --------------------------------------------------------------------

# age data, by year and type
data_raw_ac <- read.csv('./data/Pertussis_ac.csv') |> 
     mutate(Type = 'Cases')
data_raw_ad <- read.csv('./data/Pertussis_ad.csv') |> 
     mutate(Type = 'Deaths')

data_age <- bind_rows(data_raw_ac, data_raw_ad) |> 
     select(Areas, Age, Cases, Year, Type) |> 
     filter(Areas == 'Total' & Age != 'Total' & Age != 'Unknown') |> 
     mutate(Age_label = case_when(Age %in% c('0', '<1', '1-1', '2-2') ~ '0-2',
                                  Age %in% c('3-3', '4-4', '5-5') ~ '3-5',
                                  Age %in% c('6-6', '7-9') ~ '6-9',
                                  TRUE ~ Age),
            Age_label = factor(Age_label, levels = c('0-2', '3-5', '6-9',
                                                     '10-14', '15-24', '25-34', '35-44', '45-54', '55-64', '65+'))) |>
     group_by(Year, Type, Age_label) |>
     summarise(Cases = sum(Cases),
               .groups = 'drop') |> 
     pivot_wider(names_from = Type, values_from = Cases) |> 
     group_by(Year) |>
     mutate(CasesPer = Cases / sum(Cases))

remove(data_raw_ac, data_raw_ad)

# rate data, by year and type
data_rate <- read.csv('./data/Pertussis_rate.csv') |> 
     filter(Areas == 'Total') |>
     select(Year, Incidence, Cases, Deaths) |> 
     arrange(Year) |> 
     mutate(Start = as.Date(paste(Year, '01', '01', sep = '-')),
            End = as.Date(paste(Year, '12', '31', sep = '-'))) |> 
     pivot_longer(cols = c(Start, End), names_to = 'Type', values_to = 'Date')

# monthly data
data_mcd <- read.csv('./data/Pertussis_mcd.csv') |> 
     filter(Areas == 'Total' & Month != 'Total') |> 
     # transform month (Jan, Feb, Mar, Apr, May, June, July, Aug, Sep, Oct, Nov, Dec) to integer
     mutate(MonthNum = factor(Month, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'June', 'July', 'Aug', 'Sept', 'Oct', 'Nov', 'Dec')) |> 
                as.integer(),
            Date = as.Date(paste(Year, MonthNum, '01', sep = '-'))) |>
     select(Year, Month, MonthNum, Date, Type, Count) |>
     pivot_wider(names_from = Type, values_from = Count) |> 
     arrange(Date) |> 
     filter(Date <= '2024-10-01')
     
# plot --------------------------------------------------------------------

# epidemic curve
fig1 <- ggplot() +
     geom_line(data = data_rate, aes(x = Date, y = Incidence * 100, color = 'Incidence'), size = 1)+
     geom_line(data = data_mcd, aes(x = Date, y = Cases, color = 'Cases'), size = 1) +
     # using second y-axis
     scale_y_continuous(sec.axis = sec_axis(~ . * 0.01, name = 'Annual incidence rate,\nper 100,000'),
                        limits = c(0, NA),
                        expand = expansion(mult = c(0, 0.1))) +
     scale_x_date(breaks = seq(as.Date('2003-01-01'), as.Date('2024-10-01'), by = '2 years'),
                  limits = c(as.Date('2003-01-01'), as.Date('2024-12-31')),
                  expand = expansion(add = c(0, 0)),
                  date_labels = '%Y')+
     theme_bw() +
     theme(legend.position = 'none') +
     labs(x = 'Date',
          y = 'Monthly cases',
          color = 'Type',
          title = NULL)

# age distribution

fill_colors <- pal_npg()(length(unique(data_age$Age_label)))
names(fill_colors) <- unique(data_age$Age_label)

fig2 <- ggplot(data_age) +
     geom_col(aes(x = Year, y = Cases, fill = Age_label),
              position = "fill") +
     theme_bw()+
     theme(legend.position = 'bottom') +
     scale_x_continuous(breaks = seq(2003, 2024, 2),
                        expand = expansion(mult = c(0, 0))) +
     scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                        expand = expansion(mult = c(0, 0))) +
     scale_fill_manual(values = fill_colors) +
     labs(x = 'Year',
          y = 'Proportion of cases',
          fill = 'Age group',
          title = 'A')+
     # guides by row
     guides(fill = guide_legend(nrow = 1))

data_age_all <- data_age |> 
     group_by(Age_label) |>
     summarise(Cases = sum(Cases),
               Deaths = sum(Deaths),
               .groups = 'drop') |>
     pivot_longer(cols = c(Cases, Deaths), names_to = 'Type', values_to = 'Count') |>
     group_by(Type) |>
     mutate(Prop = Count / sum(Count),
            Age_label = factor(Age_label, levels = rev(levels(Age_label))),
            Label = paste0(Count, ' (', scales::percent(Prop, accuracy=0.01), ')')) |> 
     filter(Prop != 0)

fig3 <- ggplot(data_age_all) +
     geom_col(aes(x = Count, y = Type, fill = Age_label),
              position = 'fill',
              show.legend = FALSE) +
     geom_text(aes(x = Count, y = Type, label = Label),
               angle = 90,
               size = 2,
               show.legend = FALSE,
               position = position_fill(vjust = 0.5))+
     theme_bw()+
     theme(legend.position = 'none')+
     scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                        expand = expansion(mult = c(0, 0)))+
     scale_fill_manual(values = fill_colors)+
     labs(x = NULL,
          y = NULL,
          fill = 'Age group',
          title = 'B')+
     guides(fill = guide_legend(nrow = 1))

fig <- fig2 + fig3 + plot_layout(ncol = 1, heights = c(2, 1), guides = 'collect') &
     theme(legend.position = 'bottom')
            
# save --------------------------------------------------------------------

ggsave('fig1.pdf',
       fig1,
       width = 8,
       height = 4,
       device = cairo_pdf)

ggsave('fig2.pdf',
       fig,
       width = 8,
       height = 6,
       device = cairo_pdf)
