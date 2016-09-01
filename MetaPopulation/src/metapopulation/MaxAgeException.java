/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package metapopulation;

/**
 *
 * @author Aranka
 */
public class MaxAgeException extends RuntimeException{
  public MaxAgeException() { super(); }
  public MaxAgeException(String message) { super(message); }
  public MaxAgeException(String message, Throwable cause) { super(message, cause); }
  public MaxAgeException(Throwable cause) { super(cause); }
}
