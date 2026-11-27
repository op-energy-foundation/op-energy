{-- | paging shared by the V2 listings
 -}
module OpEnergy.Offer.Server.V1.Paging
  ( pageOffset
  )where

-- | the OFFSET the given page of a listing is read with, or 'Nothing' when
-- the page is so far in that its offset can not be represented.
--
-- The offset is the number of whole pages before this one times the size of a
-- page. A page arrives as a 'Data.OpEnergy.API.V1.Positive.Positive' 'Int',
-- so it is only bounded by @maxBound@, and worked out in 'Int' that product
-- wraps: page 4611686018427387905 of 100 wraps to offset 0 and answers the
-- first page's rows as if they were that page's, and the largest page of all
-- wraps to a negative offset, which the database rejects. Worked out as
-- 'Integer' and checked afterwards, so the product being checked is not the
-- one, which would have wrapped.
--
-- Lives in V1 so both of the V2 listings can read it without V1 having to
-- import V2.
--
-- Example:
--
-- > pageOffset 3 20 == Just 40
pageOffset :: Int -> Int -> Maybe Int
pageOffset page limit
  | offset <= toInteger (maxBound :: Int) = Just (fromInteger offset)
  | otherwise = Nothing
  where
    offset = toInteger (page - 1) * toInteger limit
