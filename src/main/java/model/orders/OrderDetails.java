package model.orders;

import model.Country;
import model.Player;

import java.io.Serializable;

/**
 * class to contain the details of the order
 */
public class OrderDetails implements Serializable {
    private String d_Command;

    private Player d_Player;
    private Country d_Country;

    private Country d_CountryToTarget;
    private int d_Armies;

    /**
     * getter method to get command
     * @return string
     */
    public String getD_Command() {
        return d_Command;
    }

    /**
     * setter method to set command
     * @param d_Command string
     */
    public void setD_Command(String d_Command) {
        this.d_Command = d_Command;
    }

    /**
     * getter method to get player
     * @return player
     */
    public Player getD_Player() {
        return d_Player;
    }


    /**
     * setter method to set the player with order
     * @param d_Player player
     */

    public void setD_Player(Player d_Player) {
        this.d_Player = d_Player;
    }

    /**
     * getter method to get country
     * @return country
     */
    public Country getD_Country() {
        return d_Country;
    }

    /**
     * setter method to set country
     * @param d_Country country
     */
    public void setD_Country(Country d_Country) {
        this.d_Country = d_Country;
    }

    /**
     * getter method to get armies
     * @return integer
     */

    public int getD_Armies() {
        return d_Armies;
    }

    /**
     * setter method to set armies in the order
     * @param d_Armies number of armies
     */
    public void setD_Armies(int d_Armies) {
        this.d_Armies = d_Armies;
    }
}
