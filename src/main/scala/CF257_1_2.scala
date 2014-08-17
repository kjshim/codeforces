import java.util.Scanner

/**
 * train 중에 최대한의 최단 거리를 줄이지 않고 1 에서 노드들 까지의 거리를 어떻게 줄일 수 있는가?
 *
 * 다익스트라 메소드 변형으로 해결 할 수 있다. 왜냐하면, 노드 v가 필수 기차역이 아니라고 하면
 * 다잌스트라 과정에 거기로 가는 경로가 새로 생기고, 이건 빼도 상관 없는 노드가 되는 것임
 *
 * TODO: 피보나치 힙 공부하기
 */
object CF257_1_2 extends App {
  case class Train(v:Int, w:Long)
  case class Road(u:Int, v:Int, w:Long)

  def Process(n:Int, trains:Seq[Train], roads:Seq[Road]):Long = {
    val inf = 9999999999L
    val d = Array.fill(n + 1){inf}
    val q = collection.mutable.Set[Int]()
    val visited = collection.mutable.Set[Int]()
    val roadsMatrix = Array.fill(n+1, n+1) {inf}
    val trainVSet = trains.map(_.v).toSet
    val removableTrain = collection.mutable.Set[Int]()

    def AddV(v:Int) = {
      println(s"$v visited: $visited q: $q")
      q.remove(v)
      for(u <- (1 to n)) {
        val alt = d(v) + roadsMatrix(v)(u)

        if(d(u) >= alt) {
          d(u) = alt
          if(trainVSet.contains(u) && v != 1) {
            removableTrain.add(u)
          }
          println(s"u:$u v:$v du:${d(u)} dv: ${d(v)}")

        }
      }
      visited.add(v)
    }

    for(road <- roads) {
      roadsMatrix(road.u)(road.v) = road.w
      roadsMatrix(road.v)(road.u) = road.w
    }

    val filtered_trains = collection.mutable.ArrayBuffer[Train]()
    for(train <- trains) {
      if(train.w > roadsMatrix(1)(train.v)) {
        removableTrain.add(train.v)
      } else {
        filtered_trains.append(train)
      }
    }

    (1 to n).foreach(q.add(_))
    d(1) = 0
    AddV(1)

    for(train <- filtered_trains) {
      AddV(train.v)
    }

    while(!(q.isEmpty)) {
      // find closest u which has min dist in q
      var minValue = inf
      var minNode  = -1
      for(curNode <- q){
        if(d(curNode) < minValue) {
          minValue = d(curNode)
          minNode = curNode
        }
      }
      AddV(minNode)
    }
    removableTrain.size
  }

  val in = new Scanner(System.in)

  val n = in.nextInt()
  val m = in.nextInt()
  val k = in.nextInt()
  val roads = (1 to m).map(_ => {Road(in.nextInt(), in.nextInt(), in.nextLong())})
  val trains = (1 to k).map(_ => {Train(in.nextInt(), in.nextLong())})
  println(Process(n, trains, roads))
}
