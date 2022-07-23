# Mājokļu ietvara imports

library(data.table)

dat <- fread(
  cmd = "iconv -f ISO-8859-13 -t UTF-8 frame/Frame_maj_variables_PIAAC.csv",
  colClasses = list(
    character = c("iec2019", "atvk", "adm_terit_kods",
                  "adr_kods", "adr_kods_maja", "adr_kods_dziv")
  )
)

dat[, adm_terit_kods := substr(atvk, 1, 4)]

# dat[, .(reg_date, lubridate::mdy(sub(" .*$", "", reg_date)))]
dat[, reg_date := lubridate::mdy(sub(" .*$", "", reg_date))]

dat[grep("\"\"", adrese_ir), .(adrese_ir)]
dat[grep("\"\"", adrese_vzd), .(adrese_vzd)]

dat[grep("\"\"", adrese_ir), adrese_ir := gsub("\"\"", "\"", adrese_ir)]
dat[grep("\"\"", adrese_vzd), adrese_vzd := gsub("\"\"", "\"", adrese_vzd)]

str(dat)

fwrite(x = dat, file = "data/frame.csvy.gz", yaml = T)
