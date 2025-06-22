#-------------------------------------------------------------------------------
#Data Wrangling
#-------------------------------------------------------------------------------

data(chf_df)

typeof(chf_df)

chf_df |>
  slice(1:5) |>
  ggplot(aes(y = activity)) +
  geom_spaghetti(alpha = 0.1)


data(dti_df)

dti_df