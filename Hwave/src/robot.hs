robot (name, attack, hp) = (\mess -> mess (name, attack, hp))

name (n, _, _) = n
attack(_, n, _) = n
hp (_, _, n) = n

getName r = r name
getAttack r = r attack
getHp r = r hp

setName newName r = r (\(_, a, h)-> robot(newName, a, h))
setAttack newAttack r = r (\(n, _, h)-> robot(n, newAttack, h))
setHp newHp r = r (\(n, a, _)-> robot(n, a, newHp))

printRobot r = r (\(n,a,h) ->
  n ++ 
  "\nattack: " ++ (show a) ++ 
  "\nhp: "     ++ (show h))

damage r d = r (\(n,a,h) -> robot (n, a, h - d))

fight robot1 robot2 = damage robot2 attack
  where attack = if getHp robot1 >= 0
                 then getAttack robot1
                 else 0
