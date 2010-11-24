package com.example

import org.specs._

object ExampleSpec extends Specification with unfiltered.spec.jetty.Served {
  
  import dispatch._
  
  def setup = {
    _.filter(new Auth(new AuthService {
      def verify(login: String, password: String) = (login, password) match {
        case ("admin", "admin") => true
        case _ => false
      }
    })).filter(new App)
  }
  
  val http = new Http
  "The example app" should {
    
    "require authentication" in {
      val status = try { 
        http x (host as_str) { case (code, _, _, _) => code }
      } catch { case StatusCode(code, _) => code }
      status must_== 401
    }
    
    "respond to authenticated requests" in {
      
      val status = try { 
        http x (host as_! ("admin", "admin") as_str) { case (code, _, _, _) => code } 
      } catch { case StatusCode(code, _) => code }
      status must_== 200
    }
  }
}