package ex2

import ex2.ConferenceReviewings.Question.{ FINAL, RELEVANCE }

import scala.collection.mutable.ListBuffer as ListMutable

object ConferenceReviewings:
  enum Question:
    case RELEVANCE // ("È importante per questa conferenza?"),
    case SIGNIFICANCE // ("Produce contributo scientifico?"),
    case CONFIDENCE // ("Ti senti competente a commentarlo?");
    case FINAL; // ("É un articolo da accettare?")

  object ConferenceReviewing:
    def apply(database: ListMutable[(Int, Map[Question, Int])] = ListMutable()): ConferenceReviewing = new ConferenceReviewingImpl(database)

    trait ConferenceReviewing:
      def loadReview(article: Int, scores: Map[Question, Int]): Unit

      def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

      def orderedScores(article: Int, question: Question): List[Int]

      def averageFinalScore(article: Int): Double

      def acceptedArticles(): Set[Int]

      def sortedAcceptedArticles(): List[(Int, Double)]

      def averageWeightedFinalScoreMap(): Map[Int, Double]

    private class ConferenceReviewingImpl(val database: ListMutable[(Int, Map[Question, Int])]) extends ConferenceReviewing:

      override def loadReview(article: Int, scores: Map[Question, Int]): Unit = database.append((article, scores))

      override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
        loadReview(article, Map(Question.RELEVANCE -> relevance, Question.SIGNIFICANCE -> significance, Question.CONFIDENCE -> confidence, Question.FINAL -> fin))

      override def orderedScores(article: Int, question: Question): List[Int] =
        //        database.filter(_._1.equals(article))
        //                .flatMap(_._2.toList)
        //                .filter(_._1.equals(question))
        //                .map(_._2)
        //                .sorted
        //                .toList
        database.collect {
          case (art, map) if art == article => map.collect {
            case (que, answer) if que.equals(question) => answer
          }
        }.flatten.sorted.toList

      override def averageFinalScore(article: Int): Double =
        val finalScores = orderedScores(article, Question.FINAL)
        finalScores.sum / finalScores.size.doubleValue

      override def acceptedArticles(): Set[Int] =
        val minimumAverageFinalScore: Double => Boolean = _ >= 5
        val minimumRelevanceScore: List[Int] => Boolean = _.exists(_ >= 8)
        database.filter(el => minimumAverageFinalScore(averageFinalScore(el._1)))
                .filter(el => minimumRelevanceScore(orderedScores(el._1, RELEVANCE)))
                .map(el => el._1)
                .toSet

      override def sortedAcceptedArticles(): List[(Int, Double)] =
        acceptedArticles().map(article => (article, averageFinalScore(article)))
                          .toList
                          .sortBy(_._2)

      override def averageWeightedFinalScoreMap(): Map[Int, Double] = ???