cup fl = \mess -> mess fl

getOz aCup = aCup (\fl -> fl)

isEmpty aCup = getOz aCup == 0

drink aCup amount = 
  if dt >= 0 
  then cup dt
  else cup 0
  where oz = getOz aCup
        dt = oz - amount

