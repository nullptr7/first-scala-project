package homegrown.collections.society

import homegrown.collections.society.professional.Executive

object Something {

  def help(another: Executive) {
    println(another.workDetails)
    println(another.secrets)
  }
}
