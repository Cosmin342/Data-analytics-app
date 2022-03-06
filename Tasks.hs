{-
	PP Project 2021

	This is where you will write the implementation for the given tasks.
	You can add other modules aswell.
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module Tasks where

import Dataset
import Text.Printf
import Text.Read
import Data.List (sortBy, delete, nub)
import Data.Maybe
import Data.Array

type CSV = String
type Value = String
type Row = [Value]
type Table = [Row]

{-
	TASK SET 1
-}

-- Task 1
convert_average :: Row -> String
convert_average row = printf "%.2f" ((calculate_average (tail row)) + read (head (reverse row)) :: Float)

calculate_sum :: Row -> Float
calculate_sum = (foldr (\el acc -> if el == "" then acc else acc + (read el :: Float))) 0

calculate_average :: Row -> Float
calculate_average row = ((calculate_sum row) - read (head (reverse row)) :: Float) / 4.0

calculate_grade :: Row -> Row
calculate_grade row = (head row) : [convert_average row]

simplify :: Integer -> Table -> Table
simplify i table 
                 | table == [] = []
                 | i == 0 = ["Nume", "Punctaj Exam"] : (simplify 1 (tail table))
                 | otherwise = (calculate_grade (head table)) : (simplify (i + 1) (tail table))

compute_exam_grades :: Table -> Table
compute_exam_grades = simplify 0

-- Task 2
-- Number of students who have passed the exam:
count_passed_students :: Table -> Int
count_passed_students table = foldr (\el acc -> if (read (head (tail el)) :: Float) >= 2.5 then acc + 1 else acc) 0 (tail table)

count_students :: Table -> Float
count_students table = fromInteger (foldr (\el acc -> acc + 1) 0 (tail table)) :: Float

get_passed_students_num :: Table -> Int
get_passed_students_num table = count_passed_students (compute_exam_grades table)

convert_int_float :: Int -> Float
convert_int_float number = fromInteger (toInteger number) :: Float

convert_passed_students :: Table -> Float
convert_passed_students table = convert_int_float (count_passed_students (compute_exam_grades table))

-- Percentage of students who have passed the exam:
get_passed_students_percentage :: Table -> Float
get_passed_students_percentage table = (convert_passed_students table) / (count_students (compute_exam_grades table))

-- Average exam grade
calculate_total :: Table -> Float
calculate_total table = foldr (\el acc -> acc + (read (head (tail el)) :: Float)) 0 (tail (compute_exam_grades table))

get_exam_avg :: Table -> Float
get_exam_avg table = calculate_total table / (count_students (compute_exam_grades table))

-- Number of students who gained at least 1.5p from homework:
sum_homework :: Row -> Integer -> Float
sum_homework row i
                   | i < 2 = sum_homework (tail row) (i + 1)
                   | i >= 2 && i < 5 && (head row) /= "" = (read (head row) :: Float) + sum_homework (tail row) (i + 1)
                   | i >= 2 && i < 5 && (head row) == "" = sum_homework (tail row) (i + 1)
                   | otherwise = 0.0

get_passed_hw_num :: Table -> Int
get_passed_hw_num table = foldr (\el acc -> if (sum_homework el 0) >= 1.5 then acc + 1 else acc) 0 (tail table)

-- Task 3
convert_len_to_float :: Row -> Float
convert_len_to_float row = fromInteger (toInteger (length row)) :: Float

calc_avg :: Row -> Float
calc_avg row = (foldr (\el acc -> if el /= "" then acc + (read el :: Float) else acc) 0 row) / (convert_len_to_float row)

compute_avg_qs :: Table -> Row
compute_avg_qs table
                    | length (head table) > 0 = (printf "%.2f" (calc_avg (map head table))) : (compute_avg_qs (map tail table))
                    | otherwise = []

clean :: Table -> Table
clean table = [compute_avg_qs (map reverse (map tail (map reverse (map tail table))))]

get_avg_responses_per_qs :: Table -> Table
get_avg_responses_per_qs table = ["Q1", "Q2", "Q3", "Q4", "Q5", "Q6"] : clean (tail table)

-- Task 4
calc_sum_two_str :: String -> String -> Row -> String
calc_sum_two_str str1 str2 row = show (foldr (\el acc -> if (el == str1) || (el == "") then acc + 1 else acc) 0 row)

calc_sum :: String -> Row -> String
calc_sum chr row = show (foldr (\el acc -> if (el == chr) then acc + 1 else acc) 0 row)

calc_values :: Integer -> Row -> Row
calc_values i row = [("Q" ++ (show i)), calc_sum_two_str "0" "" row, calc_sum "1" row, calc_sum "2" row]

compute_ex_summary :: Integer -> Table -> Table
compute_ex_summary i table
                       | length (head table) > 0 = (calc_values i (map head table)) : (compute_ex_summary (i + 1) (map tail table))
                       | otherwise = []

summary :: Table -> Table
summary table = compute_ex_summary 1 (map reverse (map tail (map reverse (map tail table))))

get_exam_summary :: Table -> Table
get_exam_summary table = ["Q", "0", "1", "2"] : summary (tail table)

-- Task 5
add_row :: Row -> Table -> Table
add_row row [] = [row]
add_row row (t:ts)
                   | (read (head (reverse row)) :: Float) < (read (head (reverse t)) :: Float) = row : (t:ts)
                   | (read (head (reverse row)) :: Float) == (read (head (reverse t)) :: Float) && (head row) < (head t) = row : (t:ts)
                   | otherwise = t : (add_row row ts)

rank_sort :: Table -> Table
rank_sort [row] = [row]
rank_sort (row:rest) = add_row row (rank_sort rest)

get_ranking :: Table -> Table
get_ranking table = (head (compute_exam_grades table)) : (rank_sort (tail (compute_exam_grades table)))

-- Task 6
header_6 = ["Nume", "Punctaj interviu", "Punctaj scris", "Diferenta"]

calc_new_row :: Table -> Row
calc_new_row table = [head (head table), calc_inter table, calc_write_exam table, calc_diff_col table]

convert_diff :: String -> String -> Float -> String
convert_diff int_grade write_grade aux = printf "%.2f" (aux * ((read int_grade :: Float) - (read write_grade :: Float)))

condition :: String -> String -> Bool
condition int_grade write_grade = (read int_grade :: Float) - (read write_grade :: Float) > 0

calc_diff :: String -> String -> String
calc_diff int_grade write_grade
                               | (condition int_grade write_grade) = convert_diff int_grade write_grade 1
                               | otherwise = convert_diff int_grade write_grade (-1)

calc_inter :: Table -> String
calc_inter table = printf "%.2f" (calculate_average (tail (head table)))

calc_write_exam :: Table -> String
calc_write_exam table = printf "%.2f" (read (head (reverse (head table))) :: Float)

calc_diff_col :: Table -> String
calc_diff_col table = calc_diff (calc_inter(table)) (head (reverse (head table)))

calc_exam_values :: Table -> Table
calc_exam_values [] = []
calc_exam_values table = (calc_new_row table) : (calc_exam_values (tail table))

get_exam_diff_table :: Table -> Table
get_exam_diff_table table = header_6 : (rank_sort (calc_exam_values (tail table)))


{-
  TASK SET 2
-}
splitBy:: Char -> String -> Row
splitBy carac = foldl (\acc c -> if c == carac then acc ++ [[]] else reverse (tail (reverse acc)) ++ [(head (reverse acc)) ++ [c]]) [[]]

read_csv :: CSV -> Table
read_csv csv = map (splitBy ',') (splitBy '\n' csv)

unify_rows :: Table -> Row
unify_rows table = map (reverse.tail.reverse) (map (foldr (\el acc -> el ++ "," ++ acc) []) table)

write_csv :: Table -> CSV
write_csv table = reverse (tail (reverse (foldr (\el acc -> el ++ "\n" ++ acc) [] (unify_rows table))))

col_num :: String -> Table -> Integer -> Integer
col_num name table acc
                      | (head (head table)) == name = acc
                      | otherwise = col_num name (map tail table) (acc + 1)

extract_list :: String -> Table -> Integer -> [String]
extract_list name table number
                              | number /= 0 = extract_list name (map tail table) (number - 1)
                              | otherwise = tail (map head table)

as_list :: String -> Table -> [String]
as_list name table = extract_list name table (col_num name table 0)

check_is_int :: String -> Bool
check_is_int str = (readMaybe str :: Maybe Int) /= Nothing

convert_maybe_str :: String -> Int
convert_maybe_str str = fromJust (readMaybe str :: Maybe Int)

check_int_lt :: Row -> Row -> Bool
check_int_lt row1 row2 = (convert_maybe_str (head row1)) < (convert_maybe_str (head row2))

check_int_eq :: Row -> Row -> Bool
check_int_eq row1 row2 = (convert_maybe_str (head row1)) == (convert_maybe_str (head row2))

check_int_gt :: Row -> Row -> Bool
check_int_gt row1 row2 = (convert_maybe_str (head row1)) > (convert_maybe_str (head row2))

comp_col :: Integer -> Integer -> String -> String -> Row -> Row -> Ordering
comp_col ind ini name1 name2 row1 row2
                      | (ind /= 0) && (ind == ini) = comp_col (ind - 1) ini (head row1) (head row2) (tail row1) (tail row2)
                      | (ind /= 0) && (ind /= ini) = comp_col (ind - 1) ini name1 name2 (tail row1) (tail row2)
                      | (ind == 0) && (check_is_int (head row1)) && (check_is_int (head row2)) && (check_int_lt row1 row2) = LT
                      | (ind == 0) && (check_is_int (head row1)) && (check_is_int (head row2)) && (check_int_eq row1 row2) && (name1 < name2) = LT
                      | (ind == 0) && (check_is_int (head row1)) && (check_is_int (head row2)) && (check_int_eq row1 row2) && (name1 == name2) = EQ
                      | (ind == 0) && (check_is_int (head row1)) && (check_is_int (head row2)) && (check_int_gt row1 row2) = GT
                      | (ind == 0) && (head row1 < head row2) = LT
                      | (ind == 0) && (head row1 == head row2) && (name1 < name2) = LT
                      | (ind == 0) && (head row1 == head row2) && (name1 == name2) = EQ
                      | otherwise = GT

tsort :: String -> Table -> Table
tsort name table = (head table) : (sortBy (comp_col (col_num name table 0) (col_num name table 0) "" "") (tail table))

vmap :: (Value -> Value) -> Table -> Table
vmap func table = map (map func) table

rmap :: (Row -> Row) -> [String] -> Table -> Table
rmap func header table = header : (map func (tail table))

get_hw_grade_total :: Row -> Row
get_hw_grade_total row = (head row) : [printf "%.2f" (foldr (\el acc -> if el == "" then acc else acc + (read el :: Float)) 0 (tail (tail row)))]

vunion :: Table -> Table -> Table
vunion t1 t2
            | (head t1) == (head t2) = t1 ++ (tail t2)
            | otherwise = t1

list_compl :: Int -> Row
list_compl len = take len (repeat "")

grow :: Table -> Int -> Table
grow table len = table ++ (take (len - (length table)) (repeat (list_compl (length (head table)))))

hunion :: Table -> Table -> Table
hunion t1 t2 = zipWith (++) t1 (grow t2 (length t1))

grow_rows :: Table -> Integer -> Table
grow_rows [] len = []
grow_rows (x:xs) len
                    | (length x) < (fromInteger len) = (x ++ (list_compl ((fromInteger len)- (length x)))) : (grow_rows xs len)
                    | otherwise = x : (grow_rows xs len)

cmp_elements :: Row -> Integer -> Row -> Integer -> Bool
cmp_elements row1 ind1 row2 ind2 = (row1 !! (fromInteger ind1)) == (row2 !! (fromInteger ind2))

clean_row :: Row -> Row -> Integer -> Row
clean_row row1 row2 num2 = row1 ++ (delete (row2 !! (fromInteger num2)) row2)

add_rest_row :: String -> Table -> Row -> Integer -> Integer -> Table
add_rest_row col_name [] row num1 num2 = []
add_rest_row col_name table row num1 num2
                                          | cmp_elements (head table) num1 row num2 = (clean_row (head table) row num2) : (tail table)
                                          | otherwise = (head table) : (add_rest_row col_name (tail table) row num1 num2)

join_tables :: String -> Table -> Table -> Integer -> Integer -> Integer -> Table
join_tables col_name t1 [] num1 num2 len = (grow_rows t1 len)
join_tables col_name t1 (x:xs) num1 num2 len = join_tables col_name (add_rest_row col_name t1 x num1 num2) xs num1 num2 len

head_len :: Table -> Table -> String -> Integer
head_len t1 t2 col_name = toInteger (length ((head t1) ++ (delete col_name (head t2))))

clean_head :: Table -> Table -> String -> Row
clean_head t1 t2 col_name = (head t1) ++ (delete col_name (head t2))

tjoin :: String -> Table -> Table -> Table
tjoin col_name t1 t2 = (clean_head t1 t2 col_name) :
    (join_tables col_name (tail t1) (tail t2) (col_num col_name t2 0) (col_num col_name t1 0) (head_len t1 t2 col_name))

cartesian :: (Row -> Row -> Row) -> [String] -> Table -> Table -> Table
cartesian func header t1 t2 = header : (foldr (\el acc -> (map (func el) (tail t2)) ++ acc) [] (tail t1))

projection :: [String] -> Table -> Table
projection names table
                      | ((length (head table) == 1) && (elem (head (head table)) names == True)) = table
                      | (length (head table) == 1) = (map tail table)
                      | elem (head (head table)) names == True = zipWith (:) (map head table) (projection names (map tail table))
                      | otherwise = projection names (map tail table)

{-
  TASK SET 3
-}

data QResult = CSV CSV | Table Table | List [String]

data Query =
    FromCSV CSV
    | ToCSV Query
    | AsList String Query
    | Sort String Query
    | ValueMap (Value -> Value) Query
    | RowMap (Row -> Row) [String] Query
    | VUnion Query Query
    | HUnion Query Query
    | TableJoin String Query Query
    | Cartesian (Row -> Row -> Row) [String] Query Query
    | Projection [String] Query
    | forall a. FEval a => Filter (FilterCondition a) Query
    | Graph EdgeOp Query

class Eval a where
    eval :: a -> QResult

data FilterCondition a =
    Eq String a |
    Lt String a |
    Gt String a |
    In String [a] |
    FNot (FilterCondition a) |
    FieldEq String String


class FEval a where
    feval :: [String] -> (FilterCondition a) -> FilterOp 

type FilterOp = Row -> Bool

-- where EdgeOp is defined:
type EdgeOp = Row -> Row -> Maybe Value

-- Part 1

instance Show QResult where
    show (Table table) = write_csv table
    show (CSV csv) = show csv
    show (List l) = show l

extract_table :: QResult -> Table
extract_table (Table table) = table

extract_head_table :: QResult -> Row
extract_head_table (Table table) = head table


instance Eval Query where
    eval (FromCSV csv) = Table (read_csv csv)
    eval (ToCSV query) = CSV (write_csv (extract_table (eval query)))
    eval (AsList col_name query) = List (as_list col_name (extract_table (eval query)))
    eval (Sort col_name query) = Table (tsort col_name (extract_table (eval query)))
    eval (ValueMap func query) = Table (vmap func (extract_table (eval query)))
    eval (RowMap func header query) = Table (rmap func header (extract_table (eval query)))
    eval (VUnion query1 query2) = Table (vunion (extract_table (eval query1)) (extract_table (eval query2)))
    eval (HUnion query1 query2) = Table (hunion (extract_table (eval query1)) (extract_table (eval query2)))
    eval (TableJoin str query1 query2) = Table (tjoin str (extract_table (eval query1)) (extract_table (eval query2)))
    eval (Cartesian func header query1 query2) = Table (cartesian func header (extract_table (eval query1)) (extract_table (eval query2)))
    eval (Projection header query) = Table (projection header (extract_table (eval query)))
    eval (Filter condition query) = Table ((head (extract_table (eval query))) : (clean_filtered query condition))
    eval (Graph func query) = Table (graph_cart func (extract_table (eval query)))

-- Part 2

det_num_col :: [String] -> String -> Integer -> Integer
det_num_col header str i
                         | (head header) == str = i
                         | otherwise = det_num_col (tail header) str (i + 1)

check_in_list_float :: [Float] -> Row -> String -> Row -> Bool
check_in_list_float values row str header = elem (read (extr_str_row row str header) :: Float) values

extr_str_row :: Row -> String -> Row -> String
extr_str_row row col header = row !! (fromInteger (det_num_col header col 0))

check_str_cols :: Row -> String -> String -> Row -> Bool
check_str_cols row col1 col2 header = (extr_str_row row col1 header) == (extr_str_row row col2 header)

check_float_cols :: Row -> String -> String -> Row -> Bool
check_float_cols row col1 col2 header = (read (extr_str_row row col1 header) :: Float) == (read (extr_str_row row col2 header) :: Float)

check_not_in :: Row -> String -> Row -> [Float] -> Bool
check_not_in row str header values = if (extr_str_row row str header /= "") then (not (check_in_list_float values row str header)) else True

instance FEval Float where
    feval header (Eq str value) = \row -> (read (extr_str_row row str header) :: Float) == value
    feval header (Gt str value) = \row -> (read (extr_str_row row str header) :: Float) > value
    feval header (Lt str value) = \row -> (read (extr_str_row row str header) :: Float) < value
    feval header (In str values) = \row -> if (extr_str_row row str header /= "") then check_in_list_float values row str header else False
    feval header (FieldEq col1 col2) = \row -> (check_float_cols row col1 col2 header)
    feval header (FNot (Eq str value)) = \row -> not ((read (extr_str_row row str header) :: Float) == value)
    feval header (FNot (Gt str value)) = \row -> not ((read (extr_str_row row str header) :: Float) > value)
    feval header (FNot (Lt str value)) = \row -> not ((read (extr_str_row row str header) :: Float) < value)
    feval header (FNot (In str values)) = \row -> check_not_in row str header values
    feval header (FNot (FieldEq col1 col2)) = \row -> not (check_float_cols row col1 col2 header)

instance FEval String where
    feval header (Eq str value) = \row -> (extr_str_row row str header) == value
    feval header (Gt str value) = \row -> (extr_str_row row str header) > value
    feval header (Lt str value) = \row -> (extr_str_row row str header) < value
    feval header (In str values) = \row -> elem (extr_str_row row str header) values
    feval header (FieldEq col1 col2) = \row -> (check_str_cols row col1 col2 header)
    feval header (FNot (Eq str value)) = \row -> not (extr_str_row row str header == value)
    feval header (FNot (Gt str value)) = \row -> not (extr_str_row row str header > value)
    feval header (FNot (Lt str value)) = \row -> not (extr_str_row row str header < value)
    feval header (FNot (In str values)) = \row -> not (elem (extr_str_row row str header) values)
    feval header (FNot (FieldEq col1 col2)) = \row -> not (check_str_cols row col1 col2 header)

-- Part 3

arrange :: Row -> Row
arrange (x:y:ys)
                | x > y = (y:x:ys)
                | otherwise = (x:y:ys)

check_row :: Row -> Bool
check_row (x:y:ys)
                  | x == y = True
                  | otherwise = False

clear_graph :: Table -> Table
clear_graph [] = []
clear_graph (x:xs)
                  | (elem (arrange x) xs == True) = clear_graph xs
                  | otherwise = (x : (clear_graph xs)) 

header_graph = ["From", "To", "Value"]

apply_edgeop :: Table -> EdgeOp -> [[Maybe Value]]
apply_edgeop table func = map (:[]) (foldr (\el acc -> (map (func el) (tail table)) ++ acc) [] (tail table))

clean_nothing :: [[Maybe Value]] -> Table
clean_nothing table = foldr (\el acc -> if (isJust (head el) == False) then ["!"] : acc else [fromJust (head el)] : acc) [] table

add_nodes :: Table -> Table
add_nodes table = foldr (\el acc -> (map ([head el] ++) (map (:[]) (map head (tail table)))) ++ acc) [] (tail table)

check_if_bad :: Row -> Bool
check_if_bad row = head (reverse row) == "!"

remove_bad_edges :: Table -> Table
remove_bad_edges table = nub (foldr (\el acc -> if (check_if_bad el) then acc else (if (check_row el) then acc else ((arrange el) : acc))) [] table)

graph_cart :: EdgeOp -> Table -> Table
graph_cart func table = (header_graph : (remove_bad_edges (zipWith (++) (add_nodes table) (clean_nothing (apply_edgeop table func)))))

check_filter :: (FEval a1, Eval a2) => Row -> a2 -> FilterCondition a1 -> Bool
check_filter row query condition = feval (extract_head_table (eval query)) condition row == True

clean_filtered :: (FEval a1, Eval a2) => a2 -> FilterCondition a1 -> Table
clean_filtered query condition = foldr (\el acc -> if (check_filter el query condition) then el:acc else acc) [] (tail (extract_table (eval query)))

-- Part 4

query_tab1 = (FromCSV lecture_grades_csv)
query_tab2 = (FromCSV email_map_csv)

distance :: Row -> Row -> Integer -> Integer
distance [] [] counter = counter - 1
distance _ [] counter = counter - 1
distance [] _ counter = counter - 1
distance (x:xs) (y:ys) counter
                               | x == y = distance xs ys (counter + 1)
                               | otherwise = distance xs ys counter

edgeop_quest row1 row2
            | (distance (tail row1) (tail row2) 0) >= 5 = Just (show (distance (tail row1) (tail row2) 0))
            | otherwise = Nothing

similarities_query = Sort "Value" (Graph edgeop_quest (Filter (FNot (Eq "Email" "")) (TableJoin "Email" query_tab1 query_tab2)))

{-
  TASK SET 4
-}

dist_lev :: String -> String -> Int
dist_lev s1 s2 = table ! (l1, l2)
    where
    (l1, l2) = (length s1, length s2)
    
    table = array bounds [((i, j), dist (i, j)) | (i, j) <- range bounds]
    bounds  = ((0, 0), (l1, l2))
    
    dist (i, j)
              | i == 0 = j
              | j == 0 = i
              | s1 !! (i - 1) == s2 !! (j - 1) = table ! (i - 1, j - 1)
              | otherwise = 1 + (minimum [table ! (i - 1, j), table ! (i, j - 1), table ! (i - 1, j - 1)])

extr_col :: CSV -> String -> Table
extr_col table col_name = extract_table $ eval $ Projection [col_name] $ FromCSV table

best_match :: [Integer] -> Table -> Integer -> Integer -> Integer -> String
best_match [] table min ind start = head (table !! (fromInteger ind))
best_match (x:xs) table min ind start
                               | x < min = best_match xs table x start (start + 1)
                               | otherwise = best_match xs table min ind (start + 1)

calc_diff_str :: Row -> Table -> [Integer]
calc_diff_str row table = foldr (\el acc -> (toInteger (dist_lev (head row) (head el))) : acc) [] table

dist_calc :: Table -> Table -> Table
dist_calc table1 table2 = foldr (\el acc -> [best_match (calc_diff_str el table2) table2 (toInteger (maxBound :: Int)) 0 0] : acc) [] table1

correct_cols :: String -> CSV -> CSV -> Table
correct_cols col_name table1 table2 = dist_calc (extr_col table1 col_name) (extr_col table2 col_name)

correct_table :: String -> CSV -> CSV -> CSV
correct_table col_name table1 table2 = write_csv (zipWith (++) (correct_cols col_name table1 table2) (map tail (extract_table $ eval $ FromCSV table1)))

grades_schema = ["Nume", "Punctaj Teme", "Punctaj Curs", "Punctaj Exam", "Punctaj Total"]

homework_grade :: Row -> String
homework_grade row = printf "%.2f" (foldr (\el acc -> if el == "" then acc else (read el :: Float) + acc) 0 (tail row))

homework_grades :: Table -> Table
homework_grades table = ["Nume", "Punctaj Teme"] : (foldr (\el acc -> ((head el) : [homework_grade el]) : acc) [] (tail table))

grades_exam :: Table -> Table
grades_exam table = ["Nume", "Punctaj Exam"] : (foldr (\el acc -> ((head el) : [convert_average el]) : acc) [] (tail table))

calc_grade_sum :: Row -> Float
calc_grade_sum row = 2 * (foldr (\el acc -> if el == "" then acc else (read el :: Float) + acc) 0 (reverse $ tail $ reverse row))

calc_lecture_grade :: Row -> Float -> String
calc_lecture_grade row counter = printf "%.2f" ((calc_grade_sum row) / counter)

calc_l_grade :: Row -> Table -> Row
calc_l_grade el table = [calc_lecture_grade (tail el) (counter_cols (tail (head table)))]

calc_lecture_grades :: Table -> Table
calc_lecture_grades table = ["Nume", "Punctaj Curs"] : (foldr (\el acc -> ((head el) : (calc_l_grade el table)) : acc) [] (tail table)) 

counter_cols :: Row -> Float
counter_cols row = (foldr (\el acc -> acc + 1) 0 row) - 1

join_hw_with_lecture :: CSV -> CSV -> CSV -> Table
join_hw_with_lecture table1 table2 table3 = tjoin "Email" (read_csv table3) (map reverse (read_csv (correct_table "Nume" table1 table2)))

join_and_sort :: CSV -> CSV -> CSV -> Table
join_and_sort table1 table2 table3 = tsort "Nume" $ map reverse $ join_hw_with_lecture table1 table2 table3

filter_new_table :: CSV -> CSV -> CSV -> Table
filter_new_table table1 table2 table3 = extract_table $ eval $ Filter (FNot (Eq "Nume" "")) $ FromCSV (write_csv $ join_and_sort table1 table2 table3)

calc_new_lecture :: CSV -> CSV -> CSV -> Table
calc_new_lecture table1 table2 table3 = calc_lecture_grades (filter_new_table table1 table2 table3)

total_grade :: Row -> Integer -> Float -> Float -> String
total_grade [] ind total exam
                               | (total < 2.5) || (exam < 2.5) = "4.00"
                               | otherwise = printf "%.2f" ((min total 5) + exam)
total_grade (x:xs) ind total exam
                                | (ind == 0) && (x == "") = total_grade xs (ind + 1) total exam
                                | (ind == 0) = total_grade xs (ind + 1) (total + (read x :: Float)) exam
                                | (ind == 1) && (x == "") = total_grade xs (ind + 1) total exam
                                | ind == 1 = total_grade xs (ind + 1) (total + (read x :: Float)) exam
                                | (ind == 2) && (x == "") = total_grade xs (ind + 1) total exam
                                | otherwise = total_grade xs (ind + 1) total (read x :: Float)

obtain_last_table :: Table -> Table -> Table -> Table
obtain_last_table table1 table2 table3 = tail (tjoin "Nume" (tjoin "Nume" table1 table2) table3)

make_final_row :: Row -> Row
make_final_row row = row ++ [total_grade (tail row) 0 0 0]

total_grades :: Table -> Table -> Table -> Table
total_grades table1 table2 table3 = grades_schema : (foldr (\el acc -> (make_final_row el) : acc) [] (obtain_last_table table1 table2 table3))

obtain_hw_table :: CSV -> Table
obtain_hw_table homework = homework_grades $ extract_table $ eval (Sort "Nume" $ FromCSV homework)

obtain_exam_table :: CSV -> Table
obtain_exam_table exam = grades_exam $ extract_table $ eval (Sort "Nume" $ FromCSV exam)

grades :: CSV -> CSV -> CSV -> CSV -> CSV
grades email homework exam lecture = write_csv $ total_grades (obtain_hw_table homework) (calc_new_lecture email homework lecture) (obtain_exam_table exam)