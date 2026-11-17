-- A block comment lined up under the remark that ends the line above
-- carries that remark on, and stays under it wherever the remark goes.
retries = 3 -- per parcel
            {- doubled on a cold start -}

backoff n = n * 2 -- grows with each try
                  {- and is capped
                     by the depot -}

-- The same, where it is lined up under the line rather than the remark.
pause = wait
    where
        wait = backoff 3 -- before the next try
        {- in seconds,
           at most ten -}
