package model.player;

import model.Player;

public abstract class PlayerStrategy {

    public static Player d_User;

    public PlayerStrategy() {
    }

    public abstract String createCommand();

    public static PlayerStrategy getStrategy(String p_Strategy) {
        if (p_Strategy.equals("human")){
            return new Strategy();
        }


        throw new IllegalStateException("not a valid player type");
    }
}
