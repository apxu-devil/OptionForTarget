
xboard = getOptionChain('QQQ', '2018-06-15')


xcalls = xboard$calls

xcalls = xcalls %>% rownames_to_columns(xcalls)

xcalls$Symbol = rownames(xcalls)

xoption = xcalls %>% as.tbl %>% filter(Strike == 164)

xfullcode = xoption$Symbol

xn = nchar(xfullcode)

xstrike_flt = substr(xfullcode, xn-2, xn)
xstrike_int = substr(xfullcode, xn-7, xn-3)
xstrike = as.double(xstrike_int) + as.double(xstrike_flt)/1000

xtype = substr(xfullcode, xn-8, xn-8)

xexpdate = substr(xfullcode, xn-14, xn-9) %>% as.Date(format='%y%m%d')

xsymb = substr(xfullcode, 0, xn-15)

symbol_cut = c(Symbol = xsymb)
symbol_cut['Symbol']

symbol_cut %>% as.data.frame
