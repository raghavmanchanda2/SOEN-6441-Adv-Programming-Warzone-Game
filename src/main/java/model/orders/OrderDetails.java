package model.orders;

import model.Country;
import model.Player;

import java.io.Serializable;

public class OrderDetails implements Serializable {
    private String d_Command;

    private Player d_Player;
    private Country d_Country;

    private Country d_CountryToTarget;
    private int d_Armies;

    public String getD_Command() {
        return d_Command;
    }

    public void setD_Command(String d_Command) {
        this.d_Command = d_Command;
    }

    public Player getD_Player() {
        return d_Player;
    }



    public void setD_Player(Player d_Player) {
        this.d_Player = d_Player;
    }

    public Country getD_Country() {
        return d_Country;
    }

    public void setD_Country(Country d_Country) {
        this.d_Country = d_Country;
    }

    public Country getD_CountryToTarget() {
        return d_CountryToTarget;
    }

    public void setD_CountryToTarget(Country d_CountryToTarget) {
        this.d_CountryToTarget = d_CountryToTarget;
    }

    public int getD_Armies() {
        return d_Armies;
    }

    public void setD_Armies(int d_Armies) {
        this.d_Armies = d_Armies;
    }
}
