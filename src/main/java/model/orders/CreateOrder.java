package model.orders;

import logger.Logger;
import model.Country;
import model.MapModel;
import model.Player;

import java.io.Serializable;
import java.util.StringJoiner;

public class CreateOrder implements Serializable {
    public static MapModel d_MapModel = MapModel.getInstance();

    private Logger d_logger;

    public static Order createOrder(String[] p_PlayerCommands, Player p_Player) {
        String l_Type = p_PlayerCommands[0].toLowerCase();
        Order l_Order;
        switch (l_Type) {
            case "deploy":
                l_Order = new DeployOrder();
                l_Order.setD_orderDetails(CreateDeployOrderDetails(p_PlayerCommands, p_Player));
                break;
            default:
                l_Order = null;

        }
        return l_Order;
    }

    public static OrderDetails CreateDeployOrderDetails(String[] p_Command, Player p_Player) {
        Country l_Country = d_MapModel.getCountry(p_Command[1]);
        int l_NumberOfArmies = Integer.parseInt(p_Command[2]);
        OrderDetails l_orderDetails = new OrderDetails();
        l_orderDetails.setD_Command(StringConverter(p_Command));
        l_orderDetails.setD_Player(p_Player);
        l_orderDetails.setD_Country(l_Country);
        l_orderDetails.setD_Armies(l_NumberOfArmies);
        if(p_Player.getReinforcementArmies() > 0 && l_NumberOfArmies <= p_Player.getD_ArmiesToIssue() && l_NumberOfArmies > 0){
            p_Player.setIssuedArmies(p_Player.getD_ArmiesToIssue() - l_NumberOfArmies);
        }
        return l_orderDetails;
    }

    private static String StringConverter(String[] p_Commands) {
        StringJoiner l_Joiner = new StringJoiner(" ");
        for (int l_Index = 0; l_Index < p_Commands.length; l_Index++) {
            l_Joiner.add(p_Commands[l_Index]);
        }
        return l_Joiner.toString();
    }
}
