package model;

import java.util.ArrayList;
import java.util.List;

import business.Order.Order;

public class Player {

	private String playerName;
	//private int armiesHold = 5; // base armies
	private int baseArmies = 3;
	private int reinforcementArmies = 0; // holding countries / 3 down to
	private int bonusArmies = 0; // continent 
	private int currentArmies = baseArmies + reinforcementArmies + bonusArmies;
	private List<Country> countriesHold;
	
	
	
	private ArrayList<Order> orders_list;
	
	public Player(String playerName) {
		this.playerName = playerName;
	}
	
	
	public List<Country> getCountriesHold() {
		return countriesHold;
	}


	public void setCountriesHold(List<Country> countriesHold) {
		this.countriesHold = countriesHold;
	}

	public void addCountryHold(Country country) {
		if(this.countriesHold == null) {
			this.countriesHold = new ArrayList<>();
		}
		this.countriesHold.add(country);
	}

	public String getPlayerName() {
		return playerName;
	}


	public void setPlayerName(String playerName) {
		this.playerName = playerName;
	}


	public boolean createOrder(List<Country> map, List<Player> players) {
		return false;
	}
	
	
	public Order getNextOrder() {
		
		if (!this.orders_list.isEmpty()) {
			
			Order to_return = this.orders_list.get(0); 
			this.orders_list.remove(0);
			
			return to_return;
		} else
			return null;
	}
	
}
