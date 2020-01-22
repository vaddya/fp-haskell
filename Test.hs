module Test (report, reportAll) where

report :: [Bool] -> String
report results = report $ foldl iter (0, 0)  results
  where
    iter :: (Int, Int) -> Bool -> (Int, Int)
    iter (ok, failed) b = if b then (ok + 1, failed) else (ok, failed + 1)
    
    report :: (Int, Int) -> String
    report (ok, failed) = "OK: " ++ show ok ++ ", FAILED: " ++ show failed
   
reportAll :: [[Bool]] -> String 
reportAll = report . concat