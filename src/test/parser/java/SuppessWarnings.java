package java.lang;

import java.lang.annotation.*;
import java.lang.annotation.ElementType;
import static java.lang.annotation.ElementType.*;

@Target({TYPE, FIELD, METHOD, PARAMETER, CONSTRUCTOR,
  LOCAL_VARIABLE})

 @Retention(RetentionPolicy.SOURCE)

public @interface SuppressWarnings {

String[] value();

} 