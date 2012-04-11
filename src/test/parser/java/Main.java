/*
* In ancient times, there 
* were Monsters and Greeks
* who fought each other
*/
 
public abstract class Creature
{
     protected String name;
     //...
 
     public void attack()
     {
          System.out.println(name + " attacks!");
     }
}
 
public class Achilles extends Creature
{
     //...
}
 
public class Mino extends Creature implements Monster
{
     //...
 
     public void roar()
     {
          System.out.println(name + " roars loudly!");
     }
}
 
public interface Monster
{
    void roar();
}

public class Main
{
public static void main(String[] args)
{
     Creature aCreature;
 
     //...
 
 
     /**
     * You have to cast aCreature to type Monster because
     * aCreature is an instance of Creature, and not all
     * Creatures can Roar.  We know it is a Monster,
     * therefore it can Roar because we check in the if
     * statement below.  Both Creatures can attack, so we
     * attack regardless...just if it's a monster we'd
     * like to roar before attacking.
     */
 
     if(aCreature instanceof Monster)
     {
          Monster m=(Monster)aCreature;
          m.roar(); //It's a monster so we can roar
     }
 
     //Both can attack
     aCreature.attack();
}
}