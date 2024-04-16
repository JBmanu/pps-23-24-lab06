import ex2.ConferenceReviewings.*
import ex2.ConferenceReviewings.ConferenceReviewing.*
import org.junit.Assert.assertEquals
import org.junit.{ Before, Test }

class ConferenceReviewingTest:
  val cr: ConferenceReviewing = ConferenceReviewing()

  @Before
  def init(): Unit =
    // carico una revisione per l'articolo 1:
    // - 8 per relevance, significance e final
    // - 7 per confidence
    // si ricordi che l'ordine delle domande è: relevance, significance, confidence, final
    cr.loadReview(1, 8, 8, 6, 8) // 4.8 è il voto finale pesato (usato da averageWeightedFinalScoreMap)
    // e simile per gli altri
    cr.loadReview(1, 9, 9, 6, 9) // 5.4
    cr.loadReview(2, 9, 9, 10, 9) // 9.0
    cr.loadReview(2, 4, 6, 10, 6) // 6.0
    cr.loadReview(3, 3, 3, 3, 3) // 0.9
    cr.loadReview(3, 4, 4, 4, 4) // 1.6
    cr.loadReview(4, 6, 6, 6, 6) // 3.6
    cr.loadReview(4, 7, 7, 8, 7) // 5.6

    val map: Map[Question, Int] = Map[Question, Int]
      (Question.RELEVANCE -> 8,
       Question.SIGNIFICANCE -> 8,
       Question.CONFIDENCE -> 7, // 5.6
       Question.FINAL -> 8)

    cr.loadReview(4, map)
    cr.loadReview(5, 6, 6, 6, 10) // 6.0
    cr.loadReview(5, 7, 7, 7, 10) // 7.0

  @Test def testOrderedScores(): Unit =
    // l'articolo 2 ha preso su RELEVANCE i due voti 4,9
    assertEquals(List(4, 9), cr.orderedScores(2, Question.RELEVANCE))
    // e simile per gli altri
    assertEquals(List(6, 7, 8), cr.orderedScores(4, Question.CONFIDENCE))
    assertEquals(List(10, 10), cr.orderedScores(5, Question.FINAL))

  @Test def testAverageFinalScore(): Unit =
    // l'articolo 1 ha preso voto medio su FINAL pari a 8.5, con scarto massimo 0.01
    assertEquals(8.5, cr.averageFinalScore(1), 0.01)
    // e simile per gli altri
    assertEquals(7.5, cr.averageFinalScore(2), 0.01)
    assertEquals(3.5, cr.averageFinalScore(3), 0.01)
    assertEquals(7.0, cr.averageFinalScore(4), 0.01)
    assertEquals(10.0, cr.averageFinalScore(5), 0.01)

  @Test def testAcceptedArticles(): Unit =
    // solo gli articoli 1,2,4 vanno accettati, avendo media finale >=5 e almeno un voto su RELEVANCE >= 8
    assertEquals(Set(1, 2, 4), cr.acceptedArticles())

  @Test def testSortedAcceptedArticles(): Unit =
    // articoli accettati, e loro voto finale medio
    assertEquals(List((4, 7.0), (2, 7.5), (1, 8.5)), cr.sortedAcceptedArticles())

  @Test def optionalTestAverageWeightedFinalScore(): Unit =
    // l'articolo 1 ha media pesata finale pari a (4.8+5.4)/2 = 5,1, con scarto massimo 0.01
    assertEquals((4.8 + 5.4) / 2, cr.averageWeightedFinalScoreMap()(1), 0.1)
    // e simile per gli altri
    assertEquals((9.0 + 6.0) / 2, cr.averageWeightedFinalScoreMap()(2), 0.1)
    assertEquals((0.9 + 1.6) / 2, cr.averageWeightedFinalScoreMap()(3), 0.1)
    assertEquals((3.6 + 5.6 + 5.6) / 3, cr.averageWeightedFinalScoreMap()(4), 0.1)
    assertEquals((6.0 + 7.0) / 2, cr.averageWeightedFinalScoreMap()(5), 0.1)
    assertEquals(5, cr.averageWeightedFinalScoreMap().size)
