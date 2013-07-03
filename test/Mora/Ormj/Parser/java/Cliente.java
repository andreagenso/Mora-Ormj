import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
 @interface ColumnAnnotation {
  String colName();
  boolean isPK() default false;
}
 
public class PruebaParser {
  @ColumnAnnotation(colName="TID", isPK=true)
  private int id;
  
    @ColumnAnnotation(colName="TNOMBRE")
   private String nombre;
   @ColumnAnnotation(colName="TAPELLIDO")
  
   private String apellido;
  public int getId() {
      return id;
  } 
  public void setId(int id) {
      this.id = id;
  }
  public String getApellido() {
      return apellido;
  } 
  public void setApellido(String apellido) {
      this.apellido = apellido;
  }
  public String getNombre() {
      return nombre;
  } /*
  public void setNombre(String nombre) {
      this.nombre = nombre;
  }  */
}