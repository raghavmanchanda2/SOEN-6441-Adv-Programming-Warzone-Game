package model.orders;

import java.io.Serializable;

public abstract class Order implements Serializable {
    private String d_Type;


    /**
     * An object of OrderDetails
     */
    private OrderDetails d_orderDetails;

    /**
     * A function to get order information
     *
     * @return the order information in an object
     */
    public OrderDetails getD_orderDetails() {
        return d_orderDetails;
    }

    public void setD_orderDetails(OrderDetails p_orderDetails) {
        this.d_orderDetails = p_orderDetails;
    }
    /**
     * A function to the set Order information based on the order
     *
     * @param p_OrderInfo Order Information contained in an object of type OrderInfo
     */
    /*public void setOrderInfo(OrderInfo p_OrderInfo) {
        this.d_OrderInfo = p_OrderInfo;
    }*/

    /**
     * A function to return the type of order
     *
     * @return String which indicates the type of order
     */
    public String getType() {
        return d_Type;
    }

    /**
     * A function to set the type of order
     *
     * @param p_Type String which indicates the type of order
     */
    public void setType(String p_Type) {
        this.d_Type = p_Type;
    }

    /**
     * A function to be overridden  by the Child class
     *
     * @return false as there is not order to be executed
     */
    public abstract boolean startExecute();


    /**
     * Print the command that is executed successfully
     */
    public abstract void print();

}



