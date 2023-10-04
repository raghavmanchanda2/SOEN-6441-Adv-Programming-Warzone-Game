package model.player;

import java.io.Serializable;
import java.util.Scanner;

public class Strategy extends PlayerStrategy implements Serializable {

    private final static Scanner s = new Scanner(System.in);

    public Strategy() {
    }

    @Override
    public String createCommand() {
        return s.nextLine();
    }
}
