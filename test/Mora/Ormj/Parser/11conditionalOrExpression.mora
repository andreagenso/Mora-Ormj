/* Pruebas de conditionalOrExpression prioridades
*/

// Prioridade de ||
@Anotacion( true || false  ? 25 : true )
@Anotacion( a = true || false  ? 25 : true )

// Prioridad de && ||
//  @Anotacion( a = true || false && true ? 25 : true )
//@Anotacion( true || false && true ? 25 : true )

// Prioridad de && || |
// @Anotacion( a = true || false && true | 45456 ? 25 : true ) 
// @Anotacion( true && false || true | 23 ? 25 : true )

// Prioridad de || && | ^
// @Anotacion( true && false || true | 23 ^ true ? 25 : true )
// @Anotacion( dfdf=true && false || true | 23 ^ true ? 25 : true )

// Prioridad de || && | ^ &
// @Anotacion( false & true && false || true | 23 ^ true ? 25 : true )
// @Anotacion( dfdf=true & true && false || true | 23 ^ true ? 25 : true )

// Prioridad de || && | ^ & ==
// @Anotacion( false & true && false || true | 23 ^ true == 23 ? 25 == true && false : true )
// @Anotacion( dfdf= false & true && false || true | 23 ^ true == 23 ? 25 == true && false : true )
// @Anotacion( dfdf= (false == 23 ? 25 : true ))
//  @Anotacion( dfdf= (false  &&  false == 23 ? 25 : true ))
//  @Anotacion( dfdf= (false  &&  false == 23 | 34 ? 25 : true ))

 @Anotacion( dfdf= (false & false && true != 23243 || 23 | 34 ^ "aas" == 23243 ? 25 : true ))

@Anotacion( dfdf= (false))
// @Anotacion( dfdf= (false & 25 ))
// @Anotacion( dfdf= (false &&  true ? 25  : true )) 
package NombrePaquete;