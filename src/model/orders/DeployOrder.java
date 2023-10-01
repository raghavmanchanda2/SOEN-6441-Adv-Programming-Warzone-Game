package model.orders;

import logger.ConsoleWriter;
import logger.Logger;
import model.Country;
import model.Player;

import java.io.Serializable;

public class DeployOrder extends Order implements Serializable {

    private Logger d_logger;
    private ConsoleWriter d_consoleWriter;

    public DeployOrder() {
        super();
        d_logger = new Logger();
        d_consoleWriter = new ConsoleWriter();
        d_logger.addObserver(d_consoleWriter);
        setType("deploy");
    }

    public boolean start() {
        Country l_Country = getD_orderDetails().getD_Country();
        int l_Armies = getD_orderDetails().getD_Armies();
        d_logger.setLogMessage("************************");
        if (checkCommand()){
            l_Country.armiesDeploy(l_Armies);
            return true;
        }
        return false;
    }

    public boolean checkCommand() {
        Player l_Player = getD_orderDetails().getD_Player();
        Country l_Country = getD_orderDetails().getD_Country();
        int l_Armies = getD_orderDetails().getD_Armies();
        if (l_Player == null || l_Country == null) {
            d_logger.setLogMessage("->Enter the correct command.");
            return false;
        }
        if (!l_Player.isCaptured(l_Country)) {
            d_logger.setLogMessage("The country you entered is not assigned to you.");
            return false;
        }
        if (!l_Player.deployReinforcementArmiesFromPlayer(l_Armies)) {
            d_logger.setLogMessage("Not enough armies left to deploy.");
            return false;
        }
        return true;
    }

    public void print() {
        d_logger.setLogMessage("\nDeployed" + getD_orderDetails().getD_Armies() + "armies at" + getD_orderDetails().getD_Country().getD_countryName());
        d_logger.setLogMessage("*******************");
    }
}
