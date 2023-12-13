#############
# NOT-Viral #
#############

nonviral = rm_viral

# Positive Polarization
summary(nonviral$avg_positive_polarity)

# Creation of buckets for positive polarization
pos_low = subset(nonviral, nonviral$avg_positive_polarity <= 0.3333)
pos_med = subset(nonviral, nonviral$avg_positive_polarity > 0.3333 & nonviral$avg_negative_polarity < 0.6666)
pos_high = subset(nonviral, nonviral$avg_positive_polarity >= 0.6666)

# Number of articles classified by polarization bucket
pln = nrow(pos_low) # weakly positive articles
pmn = nrow(pos_med) # moderately positive articles
phn = nrow(pos_high) # highly positive articles

# Sum of shares classified by polarization bucket
pls = sum(pos_low$shares) # weakly postive articles
pms = sum(pos_med$shares) # moderately positive articles
phs = sum(pos_high$shares) # highly positive articles

# (shares per article) = (sum of shares) / (number of articles) 
plr = pls/pln # 1215.925
pmr = pms/pmn # 1258.984
phr = phs/phn # 1363

# Negative Polarization #########################################################

neg = abs(nonviral$avg_negative_polarity) # absolute value transformation
summary(neg)

# Creation of buckets for negative polarization
neg_low = subset(nonviral, neg <= 0.3333)
neg_med = subset(nonviral, neg > 0.3333 & neg < 0.6666)
neg_high = subset(nonviral, neg >= 0.6666)

# Number of articles 
nln = nrow(neg_low) # 5911
nmn = nrow(neg_med) # 1962
nhn = nrow(neg_high) # 28

# Sum of shares
nls = sum(neg_low$shares) # 7312791
nms = sum(neg_med$shares) # 1682809
nhs = sum(neg_high$shares) # 37139

# (shares per article) = (sum of shares) / (number of articles) 
nlr = nls/nln # 1237.15
nmr = nms/nmn # 1235.543
nhr = nhs/nhn # 1326.393


#########
# Viral #
#########

viral = viral_shares

# Positive polarization
summary(viral$avg_positive_polarity)

# Creation of buckets for positive polarization
pos_low = subset(viral, viral$avg_positive_polarity <= 0.3333)
pos_med = subset(viral, viral$avg_positive_polarity > 0.3333 & viral$avg_negative_polarity < 0.6666)
pos_high = subset(viral, viral$avg_positive_polarity >= 0.6666)

# Number of articles classified by polarization bucket (weak-pln, moderate-pmn, high-phn)
pln = nrow(pos_low) # 3695
pmn = nrow(pos_med) # 3606
phn = nrow(pos_high) # 7

# Sum of shares of articles classified by polarization bucket
pls = sum(pos_low$shares) # 4492843
pms = sum(pos_med$shares) # 4539896
phs = sum(pos_high$shares) # 9541

#rating = share / article (shares per article)
plr = pls/pln # 1215.925
pmr = pms/pmn # 1258.984
phr = phs/phn # 1363

# Negative Polarization #########################################################

neg = abs(viral$avg_negative_polarity) # transformation

# creation of buckets
neg_low = subset(viral, neg <= 0.3333)
neg_med = subset(viral, neg > 0.3333 & neg < 0.6666)
neg_high = subset(viral, neg >= 0.6666)

# number of articles
nln = nrow(neg_low) # 5911
nmn = nrow(neg_med) # 1962
nhn = nrow(neg_high) # 28

# sum of shares
nls = sum(neg_low$shares) # 7312791
nms = sum(neg_med$shares) # 1682809
nhs = sum(neg_high$shares) # 37139

#rating = share / article (shares per article)
nlr = nls/nln # 1237.15
nmr = nms/nmn # 1235.543
nhr = nhs/nhn # 1326.393

# Store results as vector 
nonviral_results = c(pln, pmn, phn, pls, pms, phs, plr, pmr, phr)
viral_results = c(nln, nmn, nhn, nls, nms, nhs, nlr, nmr, nhr)
