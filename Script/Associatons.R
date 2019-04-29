###############################################################################

# Packages:
pacman::p_load(rstudioapi, readr, caret, corrplot, e1071, dplyr, car, GGally, arules, arulesViz)

# Load data ---------------------------

# Datasets:
current_path <- getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))
rm(current_path)

#Transactions
transactions <- read.transactions(file = "datasets/ElectronidexTransactions2017.csv",
                                 format = "basket",
                                 sep = ",",
                                 header = FALSE)


#Convert transactions to dataframe
transactions_mat <- as(transactions,"matrix")

transactions_df <- as.data.frame(transactions_mat)


items <- c(transactions@itemInfo$labels)
freq <- c()
#Convert logical to integer
for (i in 1:ncol(transactions_df)){
  transactions_df[ ,i] <- as.integer(transactions_df[ ,i])
  freq <- cbind(freq,sum(transactions_df[ ,i]))}

freq
freq <- as.data.frame(freq)
colnames(freq) <- items
freq_melt <- melt(freq, varnames = c("item","frequency"))


#Product Type Frequencies####
#Laptops Frequency
laptops <- sum(freq_melt$value[which(freq_melt$variable == "LG Touchscreen Laptop")],
               freq_melt$value[which(freq_melt$variable == "Acer Aspire")],
               freq_melt$value[which(freq_melt$variable == "HP Laptop")],
               freq_melt$value[which(freq_melt$variable == "ASUS Chromebook")],
               freq_melt$value[which(freq_melt$variable == "Apple Macbook Pro")],
               freq_melt$value[which(freq_melt$variable == "Dell Laptop")],
               freq_melt$value[which(freq_melt$variable == "Eluktronics Pro Gaming Laptop")],
               freq_melt$value[which(freq_melt$variable == "Alienware Laptop")],
               freq_melt$value[which(freq_melt$variable == "HP Notebook Touchscreen Laptop PC")])

#Desktops Frequency
desktop <- sum(freq_melt$value[which(freq_melt$variable == "Lenovo Desktop Computer")],
               freq_melt$value[which(freq_melt$variable == "iMac")],
               freq_melt$value[which(freq_melt$variable == "HP Desktop")],
               freq_melt$value[which(freq_melt$variable == "ASUS Desktop")],
               freq_melt$value[which(freq_melt$variable == "Dell Desktop")],
               freq_melt$value[which(freq_melt$variable == "Intel Desktop")],
               freq_melt$value[which(freq_melt$variable == "Acer Desktop")],
               freq_melt$value[which(freq_melt$variable == "CYBERPOWER Gamer Desktop")],
               freq_melt$value[which(freq_melt$variable == "Dell 2 Desktop")])

#Monitors Frequency
monitors <- sum(freq_melt$value[which(freq_melt$variable == "Acer Monitor")],
               freq_melt$value[which(freq_melt$variable == "LG Monitor")],
               freq_melt$value[which(freq_melt$variable == "ASUS Monitor")],
               freq_melt$value[which(freq_melt$variable == "ASUS 2 Monitor")],
               freq_melt$value[which(freq_melt$variable == "Dell Monitor")],
               freq_melt$value[which(freq_melt$variable == "Samsung Monitor")],
               freq_melt$value[which(freq_melt$variable == "Sceptre Monitor")],
               freq_melt$value[which(freq_melt$variable == "ViewSonic Monitor")],
               freq_melt$value[which(freq_melt$variable == "AOC Monitor")],
               freq_melt$value[which(freq_melt$variable == "HP Monitor")])

#Mice Frequency
mice <- sum(freq_melt$value[which(freq_melt$variable == "3-Button Mouse")],
            freq_melt$value[which(freq_melt$variable == "Logitech Wireless Mouse")],
            freq_melt$value[which(freq_melt$variable == "Microsoft Basic Optical Mouse")],
            freq_melt$value[which(freq_melt$variable == "Logitech 3-button Mouse")],
            freq_melt$value[which(freq_melt$variable == "Redragon Gaming Mouse")],
            freq_melt$value[which(freq_melt$variable == "HP Wireless Mouse")],
            freq_melt$value[which(freq_melt$variable == "Generic Black 3-Button")],
            freq_melt$value[which(freq_melt$variable == "Wireless Portable Mouse")],
            freq_melt$value[which(freq_melt$variable == "Gaming Mouse Professional")],
            freq_melt$value[which(freq_melt$variable == "Slim Wireless Mouse")])

#Keyboard Frequency
keyboard <- sum(freq_melt$value[which(freq_melt$variable == "HP USB Keyboard")],
                freq_melt$value[which(freq_melt$variable == "Logitech Wireless Keyboard")],
                freq_melt$value[which(freq_melt$variable == "Rii LED Keyboard")],
                freq_melt$value[which(freq_melt$variable == "Logitech Keyboard")],
                freq_melt$value[which(freq_melt$variable == "Backlit LED Gaming Keyboard")],
                freq_melt$value[which(freq_melt$variable == "Dell Wired Keyboard")],
                freq_melt$value[which(freq_melt$variable == "Apple Wired Keyboard")],
                freq_melt$value[which(freq_melt$variable == "Apple Wireless Keyboard")],
                freq_melt$value[which(freq_melt$variable == "Apple Magic Keyboard")])

#mouse and keyboard combo Frequency
mk_combo <- sum(freq_melt$value[which(freq_melt$variable == "Logitech MK550 Wireless Wave Keyboard and Mouse Combo")],
                freq_melt$value[which(freq_melt$variable == "Logitech Desktop MK120 Mouse and keyboard Combo")],
                freq_melt$value[which(freq_melt$variable == "Logitech MK270 Wireless Keyboard and Mouse Combo")],
                freq_melt$value[which(freq_melt$variable == "Dell KM117 Wireless Keyboard & Mouse")],
                freq_melt$value[which(freq_melt$variable == "EagleTec Wireless Combo Keyboard and Mouse")],
                freq_melt$value[which(freq_melt$variable == "Microsoft Wireless Comfort Keyboard and Mouse")],
                freq_melt$value[which(freq_melt$variable == "Microsoft Wireless Desktop Keyboard and Mouse")],
                freq_melt$value[which(freq_melt$variable == "Rii LED Gaming Keyboard & Mouse Combo")],
                freq_melt$value[which(freq_melt$variable == "Logitech MK360 Wireless Keyboard and Mouse Combo")])

#Computer Headphones Frequency
comp_headphones <- sum(freq_melt$value[which(freq_melt$variable == "Zombie Gaming Headset")],
                       freq_melt$value[which(freq_melt$variable == "Logitech ClearChat Headset")],
                       freq_melt$value[which(freq_melt$variable == "Panasonic On-Ear Stereo Headphones RP-HT21")],
                       freq_melt$value[which(freq_melt$variable == "PC Gaming Headset")],
                       freq_melt$value[which(freq_melt$variable == "Kensington Headphones")],
                       freq_melt$value[which(freq_melt$variable == "Logitech Stereo Headset")],
                       freq_melt$value[which(freq_melt$variable == "Koss Home Headphones")],
                       freq_melt$value[which(freq_melt$variable == "Microsoft Headset")],
                       freq_melt$value[which(freq_melt$variable == "Ailihen Stereo Headphones")],
                       freq_melt$value[which(freq_melt$variable == "XIBERIA Gaming Headset")])

#Active Headphones Frequency
active_headphones <- sum(freq_melt$value[which(freq_melt$variable == "Apple Earpods")],
                         freq_melt$value[which(freq_melt$variable == "APIE Bluetooth Headphones")],
                         freq_melt$value[which(freq_melt$variable == "Monster Beats By Dr Dre")],
                         freq_melt$value[which(freq_melt$variable == "Otium Wireless Sports Bluetooth Headphones")],
                         freq_melt$value[which(freq_melt$variable == "Panasonic In-Ear Headphone")],
                         freq_melt$value[which(freq_melt$variable == "Philips Flexible Earhook Headphones")])

#Computer Cords Frequency
comp_cords <- sum(freq_melt$value[which(freq_melt$variable == "HDMI Cable 6ft")],
                  freq_melt$value[which(freq_melt$variable == "Ethernet Cable")],
                  freq_melt$value[which(freq_melt$variable == "Etekcity Power Extension Cord Cable")],
                  freq_melt$value[which(freq_melt$variable == "Audio Cable")],
                  freq_melt$value[which(freq_melt$variable == "VGA Monitor Cable")],
                  freq_melt$value[which(freq_melt$variable == "iPhone Charger Cable")],
                  freq_melt$value[which(freq_melt$variable == "HDMI Adapter")],
                  freq_melt$value[which(freq_melt$variable == "USB Cable")],
                  freq_melt$value[which(freq_melt$variable == "Samsung Charging Cable")])

#Accessories Frequency
accessories <- sum(freq_melt$value[which(freq_melt$variable == "Microsoft Office Home and Student 2016")],
                         freq_melt$value[which(freq_melt$variable == "Computer Game")],
                         freq_melt$value[which(freq_melt$variable == "Belkin Mouse Pad")],
                         freq_melt$value[which(freq_melt$variable == "Large Mouse Pad")])
 
#Speakers Frequency
speakers <- sum(freq_melt$value[which(freq_melt$variable == "Cambridge Bluetooth Speaker")],
                freq_melt$value[which(freq_melt$variable == "JBL Splashproof Portable Bluetooth Speaker")],
                freq_melt$value[which(freq_melt$variable == "DOSS Touch Wireless Bluetooth")],
                freq_melt$value[which(freq_melt$variable == "Logitech Multimedia Speakers")],
                freq_melt$value[which(freq_melt$variable == "Rokono Mini Speaker")],
                freq_melt$value[which(freq_melt$variable == "Cyber Acoustics")],
                freq_melt$value[which(freq_melt$variable == "Bose Companion Speakers")],
                freq_melt$value[which(freq_melt$variable == "Mackie CR Speakers")],
                freq_melt$value[which(freq_melt$variable == "Sonos")])

#Printers Frequency
printers <- sum(freq_melt$value[which(freq_melt$variable == "Epson Printer")],
                freq_melt$value[which(freq_melt$variable == "HP Wireless Printer")],
                freq_melt$value[which(freq_melt$variable == "Cannon Office Printer")],
                freq_melt$value[which(freq_melt$variable == "Brother Printer")],
                freq_melt$value[which(freq_melt$variable == "DYMO Label Manker")])

#Printer Ink Frequency
printer_ink <- sum(freq_melt$value[which(freq_melt$variable == "Epson Black Ink")],
                   freq_melt$value[which(freq_melt$variable == "HP Black & Tri-color Ink")],
                   freq_melt$value[which(freq_melt$variable == "Canon Ink")],
                   freq_melt$value[which(freq_melt$variable == "Brother Printer Toner")],
                   freq_melt$value[which(freq_melt$variable == "DYMO Labeling Tape")])
    
#Computer Stands Frequency
comp_stands <- sum(freq_melt$value[which(freq_melt$variable == "Halter Acrylic Monitor Stand")],
                   freq_melt$value[which(freq_melt$variable == "Height-Adjustable Standing Desk")],
                   freq_melt$value[which(freq_melt$variable == "Multi Media Stand")],
                   freq_melt$value[which(freq_melt$variable == "Halter Mesh Metal Monitor Stand")],
                   freq_melt$value[which(freq_melt$variable == "Full Motion Monitor Mount")])

#Computer Tablets Frequency
comp_tablets <- sum(freq_melt$value[which(freq_melt$variable == "iPad")],
                    freq_melt$value[which(freq_melt$variable == "iPad Pro")],
                    freq_melt$value[which(freq_melt$variable == "Fire HD Tablet")],
                    freq_melt$value[which(freq_melt$variable == "Samsung Galaxy Tab")],
                    freq_melt$value[which(freq_melt$variable == "Kindle")])

#ExternalHardrives Frequency
ext_hardrives <- sum(freq_melt$value[which(freq_melt$variable == "1TB Portable External Hard Drive")],
                 freq_melt$value[which(freq_melt$variable == "2TB Portable External Hard Drive")],
                 freq_melt$value[which(freq_melt$variable == "5TB Desktop Hard Drive")],
                 freq_melt$value[which(freq_melt$variable == "Slim 2TB Portable External Hard Drive")],
                 freq_melt$value[which(freq_melt$variable == "3TB Portable External Hard Drive")])

#Smart Home Devs Frequency
sh_devices <- sum(freq_melt$value[which(freq_melt$variable == "Apple TV")],
                  freq_melt$value[which(freq_melt$variable == "Google Home")],
                  freq_melt$value[which(freq_melt$variable == "Smart Light Bulb")],
                  freq_melt$value[which(freq_melt$variable == "Fire TV Stick")],
                  freq_melt$value[which(freq_melt$variable == "Roku Express")])

#------------------------------------------------------------------------------------------------------------------------
prd_type_name <- c("laptop","desktop","monitor","mice","keyboard","mouse and keyboard combo","comp headphones",
                   "active headphones","comp cords","accessories","speakers","printers","printers ink","comp stands",
                   "comp tablets","ext hardrives","smart home devs")
prd_type_freq <- c(laptops,desktop,monitors,mice,keyboard,mk_combo,comp_headphones,active_headphones,
                   comp_cords,accessories,speakers,printers,printer_ink,comp_stands,comp_tablets,
                   ext_hardrives,sh_devices)
View(prd_type) <- as.data.frame(cbind(prd_type_name,prd_type_freq))

ggplot(prd_type,aes(x=prd_type_name,y=prd_type_freq, fill=prd_type_name)) + geom_col() + 
  theme(panel.background = element_rect(fill="azure3",colour = "azure3",size = 0.5,linetype = "solid"),
        axis.text.x = element_text(angle=45))

