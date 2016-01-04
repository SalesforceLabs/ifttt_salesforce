package utils

import org.specs2.specification.BeforeExample
import play.api.Play
import play.api.test.FakeApplication

object App extends FakeApplication()

trait SingleInstance extends BeforeExample {
  def before {
    if (Play.unsafeApplication == null) Play.start(App)
  }
}