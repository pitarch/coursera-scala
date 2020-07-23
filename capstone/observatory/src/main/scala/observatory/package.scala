package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1
  type Month = Int
  type Day = Int
  type Stn = Int
  type Wban = Int
  type Latitude = Double
  type Longitude = Double
  type Distance = Double

  final case class Station(stn: Stn, wban: Wban, location: Location)

  final case class TemperatureMeasure(stn: Stn, wban: Wban, year: Int, month: Int, day: Int, temperature: Temperature)

}
