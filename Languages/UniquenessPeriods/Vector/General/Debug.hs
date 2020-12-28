-- |
-- Module      :  Languages.UniquenessPeriods.Vector.General.Debug
-- Copyright   :  (c) OleksandrZhabenko 2020
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  olexandr543@yahoo.com
--
-- Generalization of the functionality of the DobutokO.Poetry.General.Debug
-- module from the @dobutokO-poetry-general-languages@ package. Since 0.3.0.0 version
-- changed the names of most functions to more appropriate. The conversion table of
-- the old to new names are given in a file ConversionTable.txt in the main source
-- directory.

{-# LANGUAGE BangPatterns #-}

module Languages.UniquenessPeriods.Vector.General.Debug (
  -- * Pure functions
  -- ** Self-recursive pure functions and connected with them ones
  maximumElBy
  , uniqNPropertiesN
  , uniqNPropertiesNAll
  , uniqNProperties2GN
  -- ** Pure functions
  , maximumElByAll
  , maximumElGBy
  , uniquenessVariantsGN
  , maximumElByVec
  , maximumElByVecAll
  -- * IO functions
  -- ** Printing subsystem
  , toFile
  , printUniquenessG1
  , printUniquenessG1List
  -- *** With 'String'-based arguments
  , printUniquenessG1ListStr
  -- *** Auxiliary function
  , newLineEnding
) where

import Data.Print.Info
import Data.List (intersperse)
import System.IO
import qualified Data.Vector as V
import Languages.UniquenessPeriods.Vector.Auxiliary
import Languages.UniquenessPeriods.Vector.StrictV
import Languages.UniquenessPeriods.Vector.Data

-- | The function evaluates the 'V.Vector' of 'UniquenessG1' @a@ @b@ elements (related with the third argument) to retrieve the possibly maximum element
-- in it with respect to the order and significance (principality)  of the \"properties\" (represented as the functions @f :: [b] -> b@) being evaluated.
-- The most significant and principal is the \"property\", which index in the 'V.Vector' of them is the 'Int' argument (so it is the first one) of the
-- function minus 1, then less significant is the next to the left \"property\" and so on.
-- The predefined library \"properties\" or related to them functions can be found in the package @uniqueness-periods-vector-properties@.
maximumElBy ::
  (Eq a, Ord b) => Int -- ^ The quantity of the represented as functions \"properties\" to be applied from the second argument. The order is from the right to the left.
  -> V.Vector ([b] -> b) -- ^ 'V.Vector' of the represented as functions \"properties\" to be applied consequently.
  ->  UniqG2 a b -- ^ The data to be analyzed.
  -> UniquenessG1 a b -- ^ The maximum element in respect with the given parameters.
maximumElBy k vN y
 | V.null . snd .get22 $ y = error "Languages.UniquenessPeriods.Vector.General.Debug.maximumElBy: undefined for the empty second element in the tuple. "
 | compare k (V.length vN) == GT = error "Languages.UniquenessPeriods.Vector.General.Debug.maximumElBy: undefined for that amount of norms. "
 | compare k 0 == GT =
   let !maxK = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 (k - 1)) (V.unsafeIndex vN1 (k - 1))) . snd . get22 $ y
       vK = V.filter (\(_,vN2,_) -> V.unsafeIndex vN2 (k - 1) == V.unsafeIndex (secondFrom3 maxK) (k - 1)) . snd . get22 $ y in
         maximumElBy (k - 1) (V.unsafeSlice 0 (k - 1) vN) (UL2 (fst . get22 $ y,vK))
 | otherwise = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 0) (V.unsafeIndex vN1 0)) . snd . get22 $ y
{-# INLINABLE maximumElBy #-}

-- | Prints every 'String' from the list on the new line to the file. Uses 'appendFile' function inside.
toFile ::
  FilePath -- ^ The 'FilePath' to the file to be written in the 'AppendMode' (actually appended with) the information output.
  -> [String] -- ^ Each 'String' is appended on the new line to the file.
  -> IO ()
toFile file xss = (mapM_ (appendFile file) . intersperse newLineEnding $ xss) >> appendFile file newLineEnding

-- | Is used to print output specified to the 'stdout' or to the 'FilePath' specified as the inner argument in the 'Info2' parameter.
printUniquenessG1
  :: (Show a, Show b) => Info2 -- ^ A parameter to control the predefined behaviour of the printing. The 'I1' branch prints to the 'stdout' and the 'I2' - to the file.
  -> UniquenessG1 a b -- ^ The element, for which the information is printed.
  -> IO ()
printUniquenessG1 info uni
  | isI1 info =
      case (\(I1 x) -> x) info of
        A -> putStr "" -- nothing is printed
        B -> mapM_ putStrLn [show . lastFrom3 $ uni]
        C -> mapM_ putStrLn [show . firstFrom3 $ uni]
        D -> mapM_ putStrLn [show . secondFrom3 $ uni]
        E -> mapM_ putStrLn [show . lastFrom3 $ uni, show . firstFrom3 $ uni]
        F -> mapM_ putStrLn [show . lastFrom3 $ uni, show . secondFrom3 $ uni]
        G -> mapM_ putStrLn [show . firstFrom3 $ uni, show . secondFrom3 $ uni]
        _ -> mapM_ putStrLn [show . lastFrom3 $ uni, show . firstFrom3 $ uni, show. secondFrom3 $ uni]  -- the most verbose output
  | otherwise =
      case (\(I2 x) -> x) info of
        Af _ -> putStr "" -- nothing is printed
        Bf xs -> toFile xs [show . lastFrom3 $ uni]
        Cf xs -> toFile xs [show . firstFrom3 $ uni]
        Df xs -> toFile xs [show . secondFrom3 $ uni]
        Ef xs -> toFile xs [show . lastFrom3 $ uni, show . firstFrom3 $ uni]
        Ff xs -> toFile xs [show . lastFrom3 $ uni, show . secondFrom3 $ uni]
        Gf xs -> toFile xs [show . firstFrom3 $ uni, show . secondFrom3 $ uni]
        ~(Hf xs) -> toFile xs [show . lastFrom3 $ uni, show . firstFrom3 $ uni, show. secondFrom3 $ uni]  -- the most verbose output

-- | Is used to print output specified to the 'stdout' or to the 'FilePath' specified as the inner argument in the 'Info2' parameter.
printUniquenessG1List
  :: (Show a, Show b) => Info2 -- ^ A parameter to control the predefined behaviour of the printing. The 'I1' branch prints to the 'stdout' and the 'I2' - to the file.
  -> [UniquenessG1 a b] -- ^ The list of elements, for which the information is printed.
  -> IO ()
printUniquenessG1List info (y:ys)
  | isI1 info =
      case (\(I1 x) -> x) info of
        A -> putStr "" -- nothing is printed
        B -> mapM_ putStrLn . map (show . lastFrom3) $ (y:ys)
        C -> mapM_ putStrLn . map (show . firstFrom3) $ (y:ys)
        D -> mapM_ putStrLn . map (show . secondFrom3) $ (y:ys)
        E -> (putStrLn . show . lastFrom3 $ y) >> (putStrLn . show . firstFrom3 $ y) >> printUniquenessG1List info ys
        F -> (putStrLn . show . lastFrom3 $ y) >> (putStrLn . show . secondFrom3 $ y) >> printUniquenessG1List info ys
        G -> (putStrLn . show . firstFrom3 $ y) >> (putStrLn . show . secondFrom3 $ y) >> printUniquenessG1List info ys
        _ -> (putStrLn . show . lastFrom3 $ y) >> (putStrLn . show . firstFrom3 $ y) >> (putStrLn . show. secondFrom3 $ y) >> printUniquenessG1List info ys  -- the most verbose output
  | otherwise =
      case (\(I2 x) -> x) info of
        Af _ -> putStr "" -- nothing is printed
        Bf xs -> toFile xs . map (show . lastFrom3) $ (y:ys)
        Cf xs -> toFile xs . map (show . firstFrom3) $ (y:ys)
        Df xs -> toFile xs . map (show . secondFrom3) $ (y:ys)
        Ef xs -> toFile xs . map (\t -> (show (lastFrom3 t) ++ newLineEnding ++ show (firstFrom3 t))) $ ys
        Ff xs -> toFile xs . map (\t -> (show (lastFrom3 t) ++ newLineEnding ++ show (secondFrom3 t))) $ ys
        Gf xs -> toFile xs . map (\t -> (show (firstFrom3 t) ++ newLineEnding ++ show (secondFrom3 t))) $ ys
        ~(Hf xs) -> toFile xs . map (\t -> (show (lastFrom3 t) ++ newLineEnding ++ show (firstFrom3 t) ++ newLineEnding ++ show (secondFrom3 t))) $ ys  -- the most verbose output
printUniquenessG1List _ [] = return ()

-- | A variant of the 'printUniquenessG1List' where @a@ is 'Char' so that the inner third arguments in the triples are 'String's.
printUniquenessG1ListStr
  :: (Show b) => Info2 -- ^ A parameter to control the predefined behaviour of the printing. The 'I1' branch prints to the 'stdout' and the 'I2' - to the file.
  -> [UniquenessG1 Char b] -- ^ The list of elements, for which the information is printed.
  -> IO ()
printUniquenessG1ListStr info (y:ys)
  | isI1 info =
      case (\(I1 x) -> x) info of
        A -> putStr "" -- nothing is printed
        B -> mapM_ putStrLn . map lastFrom3 $ (y:ys)
        C -> mapM_ putStrLn . map (show . firstFrom3) $ (y:ys)
        D -> mapM_ putStrLn . map (show . secondFrom3) $ (y:ys)
        E -> (putStrLn . lastFrom3 $ y) >> (putStrLn . show . firstFrom3 $ y) >> printUniquenessG1ListStr info ys
        F -> (putStrLn . lastFrom3 $ y) >> (putStrLn . show . secondFrom3 $ y) >> printUniquenessG1ListStr info ys
        G -> (putStrLn . show . firstFrom3 $ y) >> (putStrLn . show . secondFrom3 $ y) >> printUniquenessG1ListStr info ys
        _ -> (putStrLn . lastFrom3 $ y) >> (putStrLn . show . firstFrom3 $ y) >> (putStrLn . show. secondFrom3 $ y) >> printUniquenessG1ListStr info ys  -- the most verbose output
  | otherwise =
      case (\(I2 x) -> x) info of
        Af _ -> putStr "" -- nothing is printed
        Bf xs -> toFile xs . map lastFrom3 $ (y:ys)
        Cf xs -> toFile xs . map (show . firstFrom3) $ (y:ys)
        Df xs -> toFile xs . map (show . secondFrom3) $ (y:ys)
        Ef xs -> toFile xs . map (\t -> (lastFrom3 t ++ newLineEnding ++ show (firstFrom3 t))) $ ys
        Ff xs -> toFile xs . map (\t -> (lastFrom3 t ++ newLineEnding ++ show (secondFrom3 t))) $ ys
        Gf xs -> toFile xs . map (\t -> (show (firstFrom3 t) ++ newLineEnding ++ show (secondFrom3 t))) $ ys
        ~(Hf xs) -> toFile xs . map (\t -> (lastFrom3 t ++ newLineEnding ++ show (firstFrom3 t) ++ newLineEnding ++ show (secondFrom3 t))) $ ys  -- the most verbose output
printUniquenessG1ListStr _ [] = return ()

-- | Auxiliary printing function to define the line ending needed to be printed by 'printUniquenessG1List' function in some cases.
newLineEnding :: String
newLineEnding
  | nativeNewline == LF = "\n"
  | otherwise = "\r\n"

-- | Variant of the 'maximumElBy' function where all the given \"properties\" are used.
-- The predefined library \"properties\" or related to them functions can be found in the package @uniqueness-periods-vector-properties@.
maximumElByAll ::
  (Eq a, Ord b, Show a, Show b) => V.Vector ([b] -> b) -- ^ 'V.Vector' of the represented as functions \"properties\" to be applied consequently.
  ->  UniqG2 a b -- ^ The data to be analyzed.
  -> UniquenessG1 a b -- ^ The maximum element according to the given \"properties\".
maximumElByAll vN = maximumElBy (V.length vN) vN
{-# INLINE maximumElByAll #-}

-- | The function evaluates
-- the generated 'V.Vector' of 'UniquenessG1' @a@ @b@ elements to retrieve the possibly maximum element in it with respect to the order and significance (principality)
-- of the \"properties\" being evaluated. The most significant and principal is the \"property\", which index in the 'V.Vector' of them is the 'Int' argument of the function
-- minus 1, then less significant is the next to the left \"property\" and so on.
maximumElGBy ::
  (Eq a, Ord b, Show a, Show b) => [a] -- ^ A list of \"whitespace symbols\" that delimits the sublists in the list to be processed.
  -> Preapp a -- ^ A parameter to specify the lists to be prepended and postpended to the given data to be processed before actual processment.
  -> Int -- ^ The quantity of the represented as functions \"properties\" to be applied from the second argument. The order is from the right to the left.
  -> V.Vector ([b] -> b) -- ^ 'V.Vector' of the represented as functions \"properties\" to be applied consequently.
  -> FuncRep [a] (V.Vector c) [b] -- ^ Since version 0.5.0.0 it includes the previous variant with data constructor 'D2', but additionally allows to use just single argument with data constructor 'U1'
  -> [a] -- ^ The data to be processed. Often it can be a 'String' of the text.
  -> UniquenessG1 a b
maximumElGBy whspss rr k vN frep xs
 | compare k (V.length vN) == GT = error "Languages.UniquenessPeriods.Vector.General.Debug.maximumElGBy: undefined for that amount of norms. "
 | compare k 0 == GT =
   let vM = uniquenessVariants2GNP (get1m rr) (get2m rr) whspss vN frep xs
       maxK = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 (k - 1)) (V.unsafeIndex vN1 (k - 1))) vM
       vK = V.filter (\(_,vN2,_) -> V.unsafeIndex vN2 (k - 1) == V.unsafeIndex (secondFrom3 maxK) (k - 1)) vM in
         maximumElBy (k - 1) (V.unsafeSlice 0 (k - 1) vN) (UL2 ([],vK))
 | otherwise = V.maximumBy (\(_,vN0,_) (_,vN1,_) -> compare (V.unsafeIndex vN0 0) (V.unsafeIndex vN1 0)) . uniquenessVariantsGN whspss rr vN frep $ xs

-- | A variant for 'uniquenessVariants2GN' and 'uniquenessVariants2GNP' with the second argument defining, which one is used.
uniquenessVariantsGN ::
  (Eq a, Ord b, Show a, Show b) => [a] -- ^ A list of \"whitespace symbols\" that delimits the sublists in the list to be processed.
  -> Preapp a -- ^ A parameter to specify the lists to be prepended and postpended to the given data to be processed before actual processment.
  -> V.Vector ([b] -> b) -- ^ 'V.Vector' of the represented as functions \"properties\" to be applied consequently.
  -> FuncRep [a] (V.Vector c) [b] -- ^ Since version 0.5.0.0 it includes the previous variant with data constructor 'D2', but additionally allows to use just single argument with data constructor 'U1'
  -> [a] -- ^ The data to be processed. Often it can be a 'String' of the text.
  -> V.Vector (UniquenessG1 a b)
uniquenessVariantsGN whspss (PA ts us) vN frep = uniquenessVariants2GNP ts us whspss vN frep
uniquenessVariantsGN whspss K vN frep = uniquenessVariants2GN whspss vN frep
{-# INLINE uniquenessVariantsGN #-}

-- | Finds out the group of maximum elements with respect of the @k@ \"properties\" (the most significant of which is the rightest one, then to the left less significant etc.),
-- which is given as the first argument, and then rearranges the input moving the elements equal by the first element in the triple to the maximum element
-- to the first element in the tuple.
--
-- The last \"property\" is the first element in the 'V.Vector' of \"properties\" (@[b] -> b@).
maximumElByVec ::
  (Eq a, Ord b, Show a, Show b) => Int -- ^ The quantity of the represented as functions \"properties\" to be applied from the second argument. The order is from the right to the left.
  -> V.Vector ([b] -> b) -- ^ 'V.Vector' of the represented as functions \"properties\" to be applied consequently.
  -> UniqG2 a b -- ^ The data to be analyzed.
  -> UniqG2 a b
maximumElByVec k vN x
 | V.null . snd . get22 $ x = x
 | otherwise = let !uniq = maximumElBy k vN x in let !snD = secondFrom3 uniq in UL2 ((\(!v1,!v2) -> ((fst . get22 $ x) ++ V.toList v1,v2)) .
      V.unstablePartition (equalSnDs snD . secondFrom3) . snd . get22 $ x)
{-# INLINE maximumElByVec #-}

equalSnDs
  :: Ord b => V.Vector b
  -> V.Vector b
  -> Bool
equalSnDs v1 v2
 | V.null v1 = V.null v2
 | V.null v2 = False
 | V.unsafeIndex v1 0 == V.unsafeIndex v2 0 = equalSnDs (V.unsafeSlice 1 (V.length v1 - 1) v1) (V.unsafeSlice 1 (V.length v2 - 1) v2)
 | otherwise = False

-- | A variant of the 'maximumElByVec' where all the given \"properties\" are used.
maximumElByVecAll ::
  (Eq a, Ord b, Show a, Show b) => V.Vector ([b] -> b) -- ^ 'V.Vector' of the represented as functions \"properties\" to be applied consequently.
  -> UniqG2 a b -- ^ The data to be analyzed.
  -> UniqG2 a b
maximumElByVecAll vN = maximumElByVec (V.length vN) vN
{-# INLINE maximumElByVecAll #-}

-- |  Finds out the @n@ (the first 'Int' argument) consequential maximum elements, and then rearranges the input moving the elements equal by the first element
-- in the triple to the maximum element to the first element in the tuple.
uniqNPropertiesN ::
  (Eq a, Ord b, Show a, Show b) => Int -- ^ A quantity of the recursive calls that returns each one a new resulting group from the rest of the data processed.
  -> Int -- ^ The quantity of the represented as functions \"properties\" to be applied from the second argument. The order is from the right to the left.
  -> V.Vector ([b] -> b) -- ^ 'V.Vector' of the represented as functions \"properties\" to be applied consequently.
  -> UniqG2 a b -- ^ The data to be analyzed.
  -> UniqG2 a b
uniqNPropertiesN n k vN y
 | n <= 0 = y
 | otherwise = uniqNPropertiesN (n - 1) k vN . maximumElByVec k vN $ y
{-# INLINABLE uniqNPropertiesN #-}

-- | A variant of the 'uniqNPropertiesN' where all the given \"properties\" are used.
uniqNPropertiesNAll ::
  (Eq a, Ord b, Show a, Show b) => Int -- ^ A quantity of the recursive calls that returns each one a new resulting group from the rest of the data processed.
  -> V.Vector ([b] -> b) -- ^ 'V.Vector' of the represented as functions \"properties\" to be applied consequently.
  -> UniqG2 a b -- ^ The data to be analyzed.
  -> UniqG2 a b
uniqNPropertiesNAll n vN = uniqNPropertiesN n (V.length vN) vN
{-# INLINE uniqNPropertiesNAll #-}

--------------------------------------------------------------------------------------------

-- | The full analyzing and processment function.
uniqNProperties2GN ::
  (Eq a, Ord b, Show a, Show b) => [a] -- ^ A list of \"whitespace symbols\" that delimits the sublists in the list to be processed.
  -> Preapp a -- ^ A parameter to specify the lists to be prepended and postpended to the given data to be processed before actual processment.
  -> Int -- ^ A quantity of the recursive calls that returns each one a new resulting group from the rest of the data processed.
  -> Int -- ^ The quantity of the represented as functions \"properties\" to be applied from the second argument. The order is from the right to the left.
  -> V.Vector ([b] -> b) -- ^ 'V.Vector' of the represented as functions \"properties\" to be applied consequently.
  -> FuncRep [a] (V.Vector c) [b] -- ^ Since version 0.5.0.0 it includes the previous variant with data constructor 'D2', but additionally allows to use just single argument with data constructor 'U1'
  -> [a] -- ^ The data to be processed. Often it can be a 'String' of the text.
  -> UniqG2 a b
uniqNProperties2GN whspss rr n k vN frep xs
 | n <= 0 = UL2 ([],V.empty)
 | otherwise = let v = uniquenessVariants2GNP (get1m rr) (get2m rr) whspss vN frep xs in uniqNPropertiesN (n - 1) k vN . maximumElByVec k vN $ (UL2 ([],v))
