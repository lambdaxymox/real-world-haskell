module Examples.Ch00.KMinima
    (
        minima
    )
    where

import Data.List

{-- snippet minima --}
-- lines beginning with "--" are comments.

minima k xs = take k (sort xs)
{-- /snippet minima --}
