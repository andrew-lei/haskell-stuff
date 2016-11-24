import Data.List

mean :: [Double] -> Double
mean values = sum values / (fromIntegral $ length values)

twoPassVariance :: Double -> [Double] -> Double
twoPassVariance mean values = foldl' (\x y -> x + (y - mean)^2) 0 values / (fromIntegral $ length values)

--twoPassCovariance :: (Double, Double) -> [(Double, Double)] -> Double
--twoPassCovariance means values = foldl' (\x y -> x + (fst y - fst means) * (snd y - snd means)) 0 values / (fromIntegral $ length values - 1)

-- This function should really only work for lists of equal length
-- But eh
twoPassCovariance :: Double -> Double -> [Double] -> [Double] -> Double
twoPassCovariance mean1 mean2 values1 values2 =
  foldl' (\x y -> x + (fst y - mean1) * (snd y - mean2)) 0 (zip values1 values2) / (fromIntegral $ min (length values1) (length values2))

data Regression = Regression {slope, intercept :: Double}
  deriving Show

linRegress :: [Double] -> [Double] -> Regression
linRegress values1 values2 = Regression slope intercept
  where
    mean1 = mean values1
    mean2 = mean values2
    var1 = twoPassVariance mean1 values1
    cov = twoPassCovariance mean1 mean2 values1 values2
    slope = cov / var1
    intercept = mean2 - cov / var1 * mean1

regPredict :: Regression -> Double -> Double
regPredict reg x = slope reg * x + intercept reg

main = do print mu1
          print var1
          print mu2
          print var2
          print cov
          print $ cov / var1 -- slope is 10
          print $ mu2 - cov / var1 * mu1 -- intercept is also about 10
          print f
          print $ regPredict f 5.0
  where
    vals1 :: [Double]
    -- 100 random numbers
    -- normally distributed, mean 10, stddev 5
    vals1 = [1.3836441368390666, 7.8060802593720826, 12.005258399330614, 14.938532753365813, 8.9762621511785063, 6.483884030384349, 10.567156025682449, 14.123608675005329, 9.1195830492458558, 17.114636252489543, 10.35550430125422, 10.866680640710575, 11.096932584271457, 14.730095929103713, 12.550425182527919, 10.864040011772589, 13.064405695133816, 13.80769317106024, 7.0672986070967507, 6.4138332426124665, 11.049384178491067, 8.7224397975109547, 3.8835971069330544, 4.4686223530304678, 14.890548825140776, 3.486294293964983, 5.4698895341441549, 6.2090475831854643, 5.8000780338458711, 8.2111386603521943, 15.099669228387871, 16.901510257772152, 6.9697459808665503, 15.483330199547439, 8.141193037944463, 10.28730026081587, 13.089059453240708, 7.7587129647147295, 9.1012055849589615, 12.208485709807633, 6.9257421341191634, 17.312138460676117, 5.5086640751221685, 16.395291606769291, 9.3704435941792159, 9.5087820059236794, 2.9950614885974218, 4.117193855851335, 1.3964432047063031, 10.17345368263409, 10.396751606125672, 15.309206003508924, 7.745916908634416, 7.1681899002462917, 5.7261556203878623, 12.457175137837199, 10.21458324590064, 9.2078944844024804, -2.9836095149182036, 13.26205225749619, 5.7815211950110843, 7.2857949466786858, 7.0405845128678646, 16.035739955841521, 22.696208108326399, 1.9214215441152724, 10.865261684066001, 11.522077444447534, 12.959567821806264, 21.413799971822918, 16.953396123656983, 18.753110411622771, 6.9049197206887225, 1.1512343259329221, 22.580959238085327, 3.8725925596499238, 12.229610360531833, 20.543067121569582, 16.795936850716298, 17.880004069195216, 15.796928473247922, 11.85876633358291, 7.488772074891143, 10.22561950853181, 10.745789792402745, 11.646282885271075, 20.870277921844625, 4.9207134396321459, 8.335466783777763, 10.792559231047347, 11.273335242523215, 9.5622217867655515, 18.133582568609533, 10.628258434191329, 8.424774656413538, 2.8156936808653974, 13.291922590275256, 5.1215569019614957, 7.9702845491455054, 11.476529472212055]
    vals2 :: [Double]
    -- vals multiplied by ten, plus random normal distribution, mean 10, stddev 5
    vals2 = [22.870344093408054, 93.807713165837782, 135.53315720634234, 151.44552055439118, 97.552383994345348, 69.436309694748033, 113.43884433899886, 155.12147244803748, 93.992294804375405, 180.61644947410949, 109.35744631537814, 123.21378638945191, 122.93712640037842, 147.34056052903165, 129.1449783207928, 114.55346651047181, 141.6092486842395, 151.18940422970994, 87.536150443374083, 78.495600338258257, 124.08804636716071, 99.984986441067733, 56.278789906088363, 62.906349575506042, 146.12633034447126, 45.826379814445197, 63.865406939911487, 73.924766041925693, 69.805942940694692, 94.223920187651146, 156.3691743386299, 182.15347893956667, 66.00054517590371, 157.44548437693371, 86.565294074412279, 111.36458674887685, 144.04470592783923, 80.023087136207891, 95.934754640085302, 132.79880860500739, 77.959915624009568, 181.46657287211292, 72.028335774492092, 181.74382934169475, 103.73882661563543, 105.92248211278594, 38.97369344189697, 51.528782241549862, 27.50795099905233, 114.89521911456798, 116.16019569474832, 164.24837356462382, 81.712326014527605, 86.160756289433436, 70.443149005697336, 136.53133742198958, 109.21438335190665, 102.53700698258567, -17.764917179795749, 139.97953995469385, 72.66489330684675, 81.521955599334092, 81.265026778142726, 162.5773092346065, 237.48196222763903, 23.313819816103646, 112.3822855191921, 126.97209346747768, 135.71296624699787, 222.6388988604551, 176.98647087385419, 194.51243617847726, 83.661914967231908, 22.681962292977637, 241.90798453366347, 39.377854987826652, 130.5565417807737, 228.04678548056933, 181.99250828777835, 185.10246290888117, 165.34295305782317, 127.92868841361822, 79.1198522862788, 106.32190835971939, 106.67724977961558, 125.64822004402407, 217.42232157133805, 64.196744159689544, 95.114099202218711, 123.26506077487726, 123.30836159191398, 97.416887893129072, 184.90998901278064, 109.72820048918027, 95.585527235256393, 33.917588731713749, 140.18426790924039, 57.225824410868768, 88.919181848771373, 117.38434170170551]
    mu1 :: Double
    mu1 = mean vals1
    var1 :: Double
    var1 = twoPassVariance mu1 vals1
    mu2 :: Double
    mu2 = mean vals2
    var2 :: Double
    var2 = twoPassVariance mu2 vals2
    cov :: Double
    cov = twoPassCovariance mu1 mu2 vals1 vals2
    f :: Regression
    f = linRegress vals1 vals2