-- 座標用dfs
-- https://atcoder.jp/contests/abc335/tasks/abc335_d

type Coord = (Int, Int)

type Path = [Coord]

type Bounds = (Coord, Coord)

-- 座標を外からまわっていく
dfs :: Bounds -> Path -> S.Set Coord -> Coord -> Coord -> Coord -> [Coord]
dfs b path visited d@(di, dj) v@(vi, vj) goal
  | v == goal = reverse path
  | otherwise =
      let v'@(vi', vj') = (vi + di, vj + dj)
          (u, d') =
            if S.member v' visited || not (inRange b v')
              then ((vi + dj, vj - di), (dj, -di))
              else (v', d)
       in dfs b (v : path) (S.insert v visited) d' u goal
