object TaskBCDEFGH {
  def main(args: Array[String]): Unit = {
    folkRiseSalary(sortAddSalary(
      salaryBehavior(
        salaryDeviation(
          List(100000.0, 150000.0, 200000.0, 80000.0, 120000.0, 75000.0),
          finalSalaryPerMonth(1200000.53, 20.0, 65027.74)
        )
      )
    ), 120000.0, 200000.0)
  }

  private def finalSalaryPerMonth(yearCash: Double,
                                   premiumSize: Double,
                                   nutrCash: Double): Double = {
    val salary = ((yearCash * (premiumSize / 100) + yearCash + nutrCash) * 0.87 / 12)
    println(s"Ежемесячный оклад сотрудника после вычета налогов: $salary")
    salary
  }

  private def salaryDeviation(salaries: List[Double],
                               salary: Double): List[Double] = {
    val salariesAll = salary :: salaries
    val avgSalary = salariesAll.sum / salariesAll.length
    println(s"Средняя зарплата: $avgSalary")
    val pntDeviation = salariesAll.map(x => (x / avgSalary - 1) * 100)
    println(s"Отклонения: $pntDeviation")
    salariesAll
  }

  private def salaryBehavior(salariesAll: List[Double]): List[Double] = {
    val resSalaries = salariesAll.map(_ + 20000.0)
    println(s"Новые зарплаты: $resSalaries")
    val resMax = resSalaries.max
    val resMin = resSalaries.min
    println(s"Новая максимальная зарплата: $resMax")
    println(s"Новая минимальная зарплата: $resMin")
    resSalaries
  }

  private def sortAddSalary(resSalaries: List[Double]): List[Double] = {
    val resSalariesWithNew = (350000.0 :: 90000.0 :: resSalaries).sorted
    println(s"Новые зарплаты с новыми зарплатами: $resSalariesWithNew")
    var flag = 1
      for (x <- resSalariesWithNew.indices)
        if (130000.0 <= resSalariesWithNew(x)) {
          if (flag == 1) {
            val resSalariesWithOne = (
              resSalariesWithNew.slice(0, x)
                ++ List(130000.0)
                ++ resSalariesWithNew.slice(x, resSalariesWithNew.length)
            )
            println(s"Зарплаты + 130: $resSalariesWithOne")
            flag = 0
          }
        }
      val resSalariesWithOne = (resSalariesWithNew :+ 130000.0).sorted
      resSalariesWithOne
  }

  private def folkRiseSalary(resSalariesWithOne: List[Double],
                              middleMin: Double,
                              middleMax: Double): Unit = {
    val folkSalary = resSalariesWithOne.filter(
      value => value >= middleMin && value <= middleMax
    )
    println(s"Сотрудники с middle зарплатой: $folkSalary")
    val indexesFolkSalary = for {
      index <- resSalariesWithOne.indices
      if (
        resSalariesWithOne(index) >= middleMin
          && resSalariesWithOne(index) <= middleMax
      )
    } yield index
    println(s"Индексы сотрудников с middle зарплатой: $indexesFolkSalary")
    val riseSalary = for {
      value <- resSalariesWithOne
    } yield value * 1.07
    println(s"Проиндексированные зарплаты сотрудников: $riseSalary")
  }

}