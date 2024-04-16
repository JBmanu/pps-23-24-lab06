package ex2

import scala.collection.mutable.{ ListBuffer => ListMutable }

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
        database.filter(_._1.equals(article))
                .flatMap(_._2.toList)
                .filter(_._1.equals(question))
                .map(_._2)
                .sorted
                .toList

      override def averageFinalScore(article: Int): Double = ???

      override def acceptedArticles(): Set[Int] = ???

      override def sortedAcceptedArticles(): List[(Int, Double)] = ???

      override def averageWeightedFinalScoreMap(): Map[Int, Double] = ???