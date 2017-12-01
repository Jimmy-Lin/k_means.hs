import System.IO
import System.Environment
import Debug.Trace

-- ********************************************************************************
-- * @David, The compiled binary can be invoked as ./k_means k file_name -Jimmy
-- ********************************************************************************
main = do
    (k:file_name:_) <- read_args 2
    data_points <- crime_locations file_name
    let (clusters,centroids) = k_means (read k :: Int) data_points
    putStr (show_clusters centroids clusters)
    return 0

-- **************************************************************************
-- * @David, This is a stub. Please replace this with your K-means -Jimmy
-- **************************************************************************
-- place (1.0,1.0) (3.0,4.0) [[(3.0,4.0)]]
--     result: [[(1.0,1.0),(3.0,4.0)]]
place :: (Floating a, Ord a) => (a,a) -> (a,a) -> [[(a,a)]] -> [[(a,a)]]
place p centroid [] = []
place p centroid (e:r) 
    | (centroid == (last e)) = (p:e):(place p centroid r)
    | otherwise = e:(place p centroid r)

-- take a set of points and a set of centroids
-- returns the newly computed K sets of points
-- kmeanse [(1.0,1.0),(3.0,4.0),(103.0,104.0)] [(0.0,0.0),(100.0,100.0)]
kmeanse :: (Floating a, Ord a) => [(a,a)] -> [(a,a)] -> [[(a,a)]]
kmeanse [] centroids = (foldr (\ x r -> ([x]:r)) [] centroids)
kmeanse (e:r) centroids =
    (place e (findmin e centroids 
                 (dist e (head centroids)) 
                 (head centroids)) 
             (kmeanse r centroids))

dist :: (Floating a, Ord a) => (a,a) -> (a,a) -> a
dist (x1, y1) (x2, y2) = sqrt((x2-x1) * (x2-x1) + (y2-y1) * (y2-y1))

centroidify :: (Floating a, Ord a) => [(a,a)] -> Int -> (a,a) -> (a,a)
centroidify (e:r) n (accx,accy)
    | (n == 0) = (accx, accy)
    | (r == []) = (accx / (fromIntegral n), accy / (fromIntegral n))
    | otherwise = centroidify r n (accx+(fst e), accy+(snd e))

-- return the centroid of the cluster in which p belongs to
findmin :: (Floating a, Ord a) => (a,a) -> [(a,a)] -> a -> (a,a) -> (a,a)
findmin p [] min c = c
findmin p (e:r) min c
    | ((dist p e) < min) = findmin p r (dist p e) e
    | otherwise = findmin p r min c

canstop :: (Floating a, Ord a, Show a) => [(a,a)] -> [(a,a)] -> Bool
canstop pre post
    | 10.0 > dist (head pre) (head post) = True
    | otherwise = trace (show [(head pre),(head post)]) False

kmeansc :: (Floating a, Ord a, Show a) => [(a,a)] -> [(a,a)] -> Int -> [(a,a)]
kmeansc points pre iter = kms points pre (foldr (\ x r -> ((centroidify x ((length x)-1) (0.0,0.0)):r)) [] (kmeanse points pre)) iter
    where  
        kms :: (Floating a, Ord a, Show a) => [(a,a)] -> [(a,a)] -> [(a,a)] -> Int -> [(a,a)]
        kms points pre post iter
            -- | (canstop pre post) = post
            | (iter == 0) = post
            | otherwise = (kmeansc points post (iter-1))
    
kmeans :: (Floating a, Ord a, Show a) => [(a,a)] -> Int -> ([[(a,a)]], [(a,a)])
--kmeans points k = kmeansc points (take k points)
kmeans points k = (b,a)
    where
        a = kmeansc points (take k points) 100
        b = foldr (\ x r -> (init x):r) [] (kmeanse points a)
-- a is the centroids and b is the points clustered by the centroids

k_means :: Int -> [(Float,Float)] -> ([[(Float,Float)]],[(Float,Float)])
k_means k points = ([[(1,2),(3,4),(0,0)],[(5,6),(7,8)]], [(3.4,5.6),(6.7,8.9)])


-- *********************************************************************
-- * @David, here are some helpers you will find useful. -Jimmy
-- *********************************************************************

-- Returns the index of the point closest to the reference point based on a distance function, a reference point, and a list of candidate points
-- Eg. find_nearest_point (dist norm_2) (0,0) centroids returns the index of the centroid closest to (0,0)
find_nearest_point :: ((Float,Float) -> (Float,Float) -> Float) -> (Float,Float) -> [(Float,Float)] -> Int
find_nearest_point distance reference points = find_nearest_point' distance reference (tail points) 0 (0,distance reference (head points))
find_nearest_point' :: ((Float,Float) -> (Float,Float) -> Float) -> (Float,Float) -> [(Float,Float)] -> Int -> (Int,Float) -> Int
find_nearest_point' _ _ [] _ (min_index,_) = min_index
find_nearest_point' distance reference (point:points) index (min_index,min_distance)
    | new_distance < min_distance = find_nearest_point' distance reference points new_index (new_index,new_distance)
    | otherwise = find_nearest_point' distance reference points new_index (min_index,min_distance)
    where
        new_distance = distance reference point
        new_index = index + 1

-- Computes the distance between two points based on a norm function
-- Eg. dist norm_2 a b computes the Euclidean distance between a and b
dist :: ((Float,Float) -> Float) -> (Float,Float) -> (Float,Float) -> Float
dist norm (a_x,a_y) (b_x,b_y) = norm (a_x-b_x,a_y-b_y)

-- Computes a centroid based on a dimension aggregator and a list of points
-- Eg. centroid mean points computes the average centroid of points
centroid :: ([Float] -> Float) -> [(Float,Float)]-> (Float,Float)
centroid aggregator points  = 
    let (x_total,y_total) = unzip_points points
    in (aggregator x_total, aggregator y_total)

-- Computes the 2-norm of a 2D tuple
norm_2 :: (Float,Float) -> Float
norm_2 (x,y) = sqrt( x^2 + y^2 )

-- Computes the average (mean) of a list of Floats
mean :: [Float] -> Float
mean list = mean' list 0 0.0
mean' [] count sum = sum/count
mean' (h:t) count sum = mean' t (count+1) (sum+h)



-- *************************************************************************************************************************
-- * Below are more lower-level and IO-related functions. These you can ignore since they won't be very useful -Jimmy
-- *************************************************************************************************************************

-- Aggregates a list of tuples into a tuple of lists, where each list is an aggregate in each dimension.
unzip_points :: [(Float,Float)] -> ([Float],[Float])
unzip_points points = unzip_points' points ([],[])
unzip_points' [] total = total
unzip_points' ((x,y):points) (x_total,y_total) = unzip_points' points ((x:x_total),(y:y_total))

show_clusters :: [(Float,Float)] -> [[(Float,Float)]] -> String
show_clusters [] _ = ""
show_clusters _ [] = ""
show_clusters (centroid:centroids) (cluster:clusters) = 
    "Cluster Size: " ++ show (length cluster) ++ "\nCluster Centroid: " ++  show centroid ++ "\nCluster Content: " ++ unwords (map show cluster) ++ "\n" ++ show_clusters centroids clusters

-- Returns an IO object containing a list of (X,Y) tuples of crime incidences. File content must be a labelled CSV with Floats at column 8 and 9
crime_locations :: String -> IO [(Float,Float)]
crime_locations file_name = do
    csv <- read_csv file_name
    return (map crime_location (tail csv))
crime_location :: [String] -> (Float,Float)
crime_location row = (read (row !! 8) :: Float, read (row !! 9) :: Float)

-- Returns an IO object of the first N command line arguments
read_args :: Int -> IO [String]
read_args n = do
    args <- getArgs
    return (take n args)

-- Returns a IO object containing a nested 2D list of CSV data, where each data item is a String
read_csv :: String -> IO [[String]]
read_csv file_name = do
    rows <- read_lines file_name
    return (split_rows rows)

-- Returns an IO object containing a list of strings where each string is a line in a file
read_lines :: String -> IO [String]
read_lines file_name = do
    text <- readFile file_name
    return (lines text)

-- Separates a list of strings into a list of list of strings (split using commas)
split_rows :: [String] -> [[String]]
split_rows rows = split_rows' rows []
split_rows' [] state = reverse state
split_rows' (row:rows) state = split_rows' rows ((split_row row):state)

-- Separates a string into a list of strings (split using commas)
split_row :: String -> [String]
split_row row = split_on ',' row 

-- Separates a string into a list of strings (split using parameterized separater character)
split_on :: Char -> String -> [String]
split_on separator list = split_on' separator list []
split_on' separator [] state = reverse state
split_on' separator list state = 
    let word = takeWhile (\x -> x /= separator) list
        remainder = drop ((length word)+1) list
    in split_on' separator remainder (word:state)
