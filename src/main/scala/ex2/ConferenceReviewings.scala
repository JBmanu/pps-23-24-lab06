package ex2

import ex2.ConferenceReviewings.Question.{ CONFIDENCE, FINAL, RELEVANCE }

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
        //                .map(_._2(question))
        //                .sorted
        //                .toList
        database.collect { case (art, map) if art == article => map(question) }
                .sorted
                .toList

      override def averageFinalScore(article: Int): Double =
        val finalScores = orderedScores(article, Question.FINAL)
        finalScores.sum / finalScores.size.doubleValue

      private def acceptArticle(i: Int): Boolean =
        averageFinalScore(i) >= 5 && orderedScores(i, RELEVANCE).exists(_ >= 8)

      override def acceptedArticles(): Set[Int] = database.map(_._1).toSet.filter(acceptArticle)

      override def sortedAcceptedArticles(): List[(Int, Double)] =
        acceptedArticles().map(article => (article, averageFinalScore(article)))
                          .toList
                          .sortBy(_._2)

      private def averageWeightedFinalScore(article: Int): Double =
        val averageWeightedFinalScores = database.filter(_._1.equals(article))
                                                 .map(el => (el._2(FINAL) * el._2(CONFIDENCE)) / 10.0d)
        averageWeightedFinalScores.sum / averageWeightedFinalScores.size

      override def averageWeightedFinalScoreMap(): Map[Int, Double] =
        database.map(_._1)
                .distinct
                .map(article => (article, averageWeightedFinalScore(article)))
                .toMap