package controller;

import logger.ConsoleWriter;
import logger.Logger;
import model.MapModel;
import model.Player;
import model.orders.Order;

public class OrderExecute {
    MapModel d_MapModel;

    private Logger d_logger;
    private ConsoleWriter d_consoleWriter;

    public OrderExecute() {
        d_MapModel = MapModel.getInstance();
        d_logger = new Logger();
        d_consoleWriter = new ConsoleWriter();
        d_logger.addObserver(d_consoleWriter);
    }

    public void orderExecute(){
        int l_Counter = 0;
        while (l_Counter < d_MapModel.getPlayers().size()) {
            l_Counter = 0;
            for (Player l_User : d_MapModel.getPlayers().values()) {
                Order l_Order = l_User.nextOrder();
                if (l_Order == null) {
                    l_Counter++;
                } else {
                    if (l_Order.startExecute()) {
                        l_Order.print();
                    }
                }
            }
        }
    }
}