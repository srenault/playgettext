package models

import java.util.Locale
import org.xnap.commons.i18n.I18n
import org.xnap.commons.i18n.I18nFactory

object I18n {
  lazy val fr = I18nFactory.getI18n(getClass, "playgettext.i18n.Messages_fr", java.util.Locale.FRENCH)
  lazy val en = I18nFactory.getI18n(getClass, "playgettext.i18n.Messages_en", java.util.Locale.ENGLISH)
}
