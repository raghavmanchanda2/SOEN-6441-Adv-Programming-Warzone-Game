package controller;

import logger.ConsoleWriter;
import logger.Logger;
import model.Country;
import model.MapModel;
import model.Player;
import model.orders.Order;

import java.util.*;

public class IssueOrder {

    private static Set<Player> skipPlayers = new HashSet<>();

    public static String PlayerCommands = null;

    MapModel d_MapModel;

    private Logger d_logger;
    private ConsoleWriter d_consoleWriter;

    public IssueOrder() {
        d_MapModel = MapModel.getInstance();
        d_logger = new Logger();
        d_consoleWriter = new ConsoleWriter();
        d_logger.addObserver(d_consoleWriter);
    }

    public void issue_order(){
        if (d_MapModel.getCurrentPlayer() == null) {
            d_MapModel.setCurrentPlayer(d_MapModel.getPlayers().entrySet().iterator().next().getValue());
        }

        while (!(skipPlayers.size() == d_MapModel.getPlayers().size())) {
            for (Player l_Player : d_MapModel.getPlayers().values()) {
                if (!(l_Player.getName().equalsIgnoreCase(d_MapModel.getCurrentPlayer().getName()))) {
                    continue;
                }
                if (!skipPlayers.isEmpty() && skipPlayers.contains(l_Player)) {
                    continue;
                }
                //d_MapModel.setGameLoaded(false);
                d_MapModel.setCurrentPlayer(l_Player);
                boolean l_IssueCommand = false;
                while (!l_IssueCommand) {
                    gameProgress(l_Player);
                    PlayerCommands = l_Player.readFromPlayer();
                    if (Objects.isNull(PlayerCommands)) {
                        PlayerCommands = "";
                    }
                    if (!PlayerCommands.isEmpty()) {
                        l_IssueCommand = checkCommand(PlayerCommands, l_Player);
                    }
                    if (PlayerCommands.equals("pass")) {
                        break;
                    }

                }
                if (!PlayerCommands.equals("pass")) {
                    d_logger.setLogMessage(l_Player.getName() + " has issued this order :- " + PlayerCommands);
                    l_Player.issueOrder();
                    d_logger.setLogMessage("The order has been added to the list of orders.");
                    d_logger.setLogMessage("=============================================================================");
                }
            }

        }
        skipPlayers.clear();

    }

    public boolean checkCommand(String p_CommandArr, Player p_Player) {
        List<String> l_Commands = Arrays.asList("deploy", "advance", "bomb", "blockade", "airlift", "negotiate", "savegame");
        String[] l_CommandArr = p_CommandArr.split(" ");
        if (p_CommandArr.toLowerCase().contains("pass")) {
            AddToSetOfPlayers(p_Player);
            return false;
        }
        if (!l_Commands.contains(l_CommandArr[0].toLowerCase())) {
            d_logger.setLogMessage("The command syntax is invalid." + p_CommandArr);
            return false;
        }

        switch (l_CommandArr[0].toLowerCase()) {
            case "deploy":
                try {
                    Integer.parseInt(l_CommandArr[2]);
                } catch (NumberFormatException l_Exception) {
                    d_logger.setLogMessage("The number format is invalid");
                    return false;
                }
                if(Integer.parseInt(l_CommandArr[2]) < 0){
                    d_logger.setLogMessage("The number format is invalid");
                    return false;
                }
                break;
            case "advance":
                try {
                    Integer.parseInt(l_CommandArr[3]);
                } catch (NumberFormatException l_Exception) {
                    d_logger.setLogMessage("The number format is invalid");
                    return false;
                }
                break;
            case "savegame":

            default:
                break;

        }
        return true;
    }

    private static void AddToSetOfPlayers(Player p_Player) {
        skipPlayers.add(p_Player);
    }


    public void gameProgress(Player p_Player) {
        d_logger.setLogMessage("*****************************");
        d_logger.setLogMessage("List of game loop commands");
        d_logger.setLogMessage("To deploy the armies : deploy countryID numarmies");
        d_logger.setLogMessage("To skip: pass");
        d_logger.setLogMessage("*****************************");
        String l_Table = "|%-15s|%-19s|%-22s|%n";
        System.out.format("+--------------+-----------------------+------------------+%n");
        System.out.format("| Current Player   | Initial Assigned  | Left Armies      | %n");
        System.out.format("+---------------+------------------  +---------------------+%n");
        System.out.format(l_Table, p_Player.getName(), p_Player.getReinforcementArmies(), p_Player.getIssuedArmies());
        System.out.format("+--------------+-----------------------+------------------+%n");

        d_logger.setLogMessage("The countries assigned to the player are: ");
        System.out.format("+--------------+-----------------------+------------------+---------+%n");

        System.out.format(
                "|Country name  |Country Armies  | Neighbors                         |%n");
        System.out.format(
                "+--------------+-----------------------+------------------+---------+%n");
        for (Country l_Country : p_Player.getCapturedCountries()) {
            String l_TableCountry = "|%-15s|%-15s|%-35s|%n";
            String l_NeighborList = "";
            for (Country l_Neighbor : l_Country.getNeighbors()) {
                l_NeighborList += l_Neighbor.getD_countryName() + "-";
            }
            System.out.format(l_TableCountry, l_Country.getD_countryName(), l_Country.getArmies(), l_Country.createNeighboursList(l_Country.getNeighbors()));
        }
        System.out.format("+--------------+-----------------------+------------------+---------+\n");


        if (!p_Player.getOrders().isEmpty()) {
            d_logger.setLogMessage("The Orders issued by Player " + p_Player.getName() + " are:");
            for (Order l_Order : p_Player.getOrders()) {
                d_logger.setLogMessage(l_Order.getD_orderDetails().getD_Command());
            }
        }
    }
}
