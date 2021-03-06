{- 
Functions used for calculating the power generated by a satellite under
various design and noise configurations, and then calculating the signal
to noise ratio for each design over the different noise possibilities.
-}

data BodyType = Cuboid | Cylinder deriving (Show)

data Design = Design Float BodyType deriving (Show)
data Noise = Noise Float Float Float deriving (Show)
data Parameters = Parameters { efficiency :: Float
                             , packingEfficiency :: Float
                             , volume :: Float
                             , solarFlux :: Float
                             }

results :: [([Float], Float)]
results = map (powersAndSignalToNoise problemParameters noiseOptions) designOptions

problemParameters :: Parameters
problemParameters = Parameters { efficiency = 0.3
                               , packingEfficiency = 0.9
                               , volume = 3
                               , solarFlux = solarFluxAt 148107600
                               }

designOptions :: [Design]
designOptions = [ Design 0.5 Cuboid
                , Design 0.5 Cylinder
                , Design 2 Cuboid
                , Design 2 Cylinder
                ]

noiseOptions :: [Noise]
noiseOptions = [ noise 0 0 0.1
               , noise 0 15 0.3
               , noise 15 0 0.3
               , noise 15 15 0.1
               ]

-- | Generate a Noise object, automatically converting the
-- angles from degrees to radians.
noise :: Float -> Float -> Float -> Noise
noise theta phi d = Noise (toRadians theta) (toRadians phi) d
    where toRadians x = (pi / 180) * x

-- | Calculate the effective area of solar panels for
-- a given design configuration and noise settings.
area :: Design -> Noise -> Float -> Float
area design@(Design _ bodyType) (Noise theta phi d) v =
    let (a, b) = dimensions design v
    in case bodyType of
        Cuboid   -> a * b * (cos theta) * (cos phi + sin phi)
        Cylinder -> a * b * (cos theta)

-- | Find the dimensions of the body from the ratio
-- of dimensions and the body type, given the required
-- body volume.
dimensions :: Design -> Float -> (Float, Float)
dimensions (Design ab bodyType) v = case bodyType of
    Cuboid   -> let b = cubeRoot (v / ab)
                    a = v / (b * b)
                in (a, b)
    Cylinder -> let b = cubeRoot $ (4 / pi) * (1 / ab) * v
                    a = (4 * v) / (pi * b * b)
                in (a, b)
    where cubeRoot x = x ** (1 / 3)

-- | Find the power generated at the end of life for a satellite
-- with the given design and noise parameters.
power :: Design -> Parameters -> Noise -> Float
power design params noise@(Noise theta phi d) =
    (solarFlux params) * efficiency' * area'
    where
        efficiency' = (efficiency params) * (packingEfficiency params) * (1 - d)
        area' = area design noise (volume params)

-- | Calculate the mean signal to noise ratio for the given
-- powers as found in a number of experiments.
signalToNoise :: [Float] -> Float
signalToNoise = (* (-10)) . logBase 10 . mean . map (\x -> 1 / (x * x))
    where mean xs = (foldr1 (+) xs) / (fromIntegral $ length xs)

-- | Find the solar flux at a the given distance from
-- the Sun, in km.
solarFluxAt :: Float -> Float
solarFluxAt d = 1365 * (149597870 / d)

-- | Return the power output by the design under each combination of
-- noise factors, as well as the mean signal to noise across all
-- combinations of noise factors.
powersAndSignalToNoise :: Parameters -> [Noise] -> Design -> ([Float], Float)
powersAndSignalToNoise params noises design =
    let powers = map (power design params) noises
        snRatio = signalToNoise powers
    in (powers, snRatio)
