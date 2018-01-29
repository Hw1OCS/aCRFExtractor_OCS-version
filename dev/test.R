
test <- data.frame(Name  = c("Nelle", "Alex", "Thomas", "Jeff", "Rodger", "Michi"),
                   Age   = c(18,18,16,16,16,17), 
                   Grade = c(1,5,2,3,1,4))

test.srt <- test %>%
  dplyr::arrange(Name) %>%
  print()

test.srt.multp <- test %>%
  dplyr::arrange(Age, Grade) %>%
  print()



##################################
## Test pkg::compareDF          ##
##################################
df1 = data.frame(id1 = c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710",
                         "Hornet 4 Drive", "Duster 360", "Merc 240D"),
                 id2 = c("Maz", "Maz", "Dat", "Hor", "Dus", "Mer"),
                 hp = c(110, 110, 181, 110, 245, 62),
                 cyl = c(6, 6, 4, 6, 8, 4),
                 qsec = c(16.46, 17.02, 33.00, 19.44, 15.84, 20.00))

df2 = data.frame(id1 = c("Mazda RX4", "Mazda RX4 Wag", "Datsun 710",
                         "Hornet 4 Drive", " Hornet Sportabout", "Valiant"),
                 id2 = c("Maz", "Maz", "Dat", "Hor", "Dus", "Val"),
                 hp = c(110, 110, 93, 110, 175, 105),
                 cyl = c(6, 6, 4, 6, 8, 6),
                 qsec = c(16.46, 17.02, 18.61, 19.44, 17.02, 20.22))

# df1 = data.frame(var1 = c("A", "B", "C"), 
#                  var2 = c(1, 2, 3))
# 
# df2 = data.frame(var1 = c("A", "B", "C"), 
#                  var2 = c(1, 2, 4))

df_compare <- compareDF::compare_df(df_new = df2, df_old = df1, group_col = c("hp"))

df_compare$html_output


