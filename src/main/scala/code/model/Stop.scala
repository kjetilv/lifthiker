package code.model

case class Stop(ID: Int,
                Name: String,
                District: String,
                Type: Int,
                ShortName: String,
                Lines: List[Line]) {

}

case class Line(LineID: Int,
                LineName: String,
                Transportation: Int)
