package code.model

case class Stop(id: Long,
                name: String,
                district: String,
                `type`: Int,
                shortName: String,
                lines: List[Line]) {

}

case class Line(lineId: Long,
                lineName: String,
                transportation: Long)
