package model;

import java.util.*;

import business.Order.Order;

/**
 * Player class which holds the issue order and next order in the list of orders
 * @author Rohit, Kevins
 * @version build 2
 */
public class Player {

	private static final int MAX_CARD_LIMIT = 3;
	
	private String playerName;
	//private int armiesHold = 5; // base armies
	private int baseArmies = 5;
	private int armiesToIssue = 5;
	private int reinforcementArmies = 0; // holding countries / 3 down to
	private int bonusArmies = 0; // continent 
	private int currentArmies = baseArmies + reinforcementArmies + bonusArmies;
	private boolean d_can_get_card_this_turn;

	private List<Country> countriesHold;
	private Map<Country,Integer> currentArmyInCountry;
	private List<Card> d_cards = new ArrayList<>();

	private Deque<Order> orders_list = new ArrayDeque<>();

	private List<Order> cardOrders_list;
	
	private Player peaceWith;
	
	public Player(String playerName) {
		this.playerName = playerName;
		d_can_get_card_this_turn = false;
	}

	public int getArmiesToIssue() {
		return armiesToIssue;
	}

	public void setArmiesToIssue(int armiesToIssue) {
		this.armiesToIssue = armiesToIssue;
	}

	public void resetArmiesToIssue() {
		this.armiesToIssue = 5;
	}

	public Order nextOrder() {
		return orders_list.poll();
	}

	public List<Country> getCountriesHold() {
		return countriesHold;
	}


	public Map<Country, Integer> getCurrentArmyInCountry() {
		return currentArmyInCountry;
	}


	public void setCurrentArmyInCountry(Map<Country, Integer> currentArmyInCountry) {
		this.currentArmyInCountry = currentArmyInCountry;
	}


	public void setCountriesHold(List<Country> countriesHold) {
		this.countriesHold = countriesHold;
	}

	public void addCountryHold(Country country) {
		if(this.countriesHold == null) {
			this.countriesHold = new ArrayList<>();
		}
		if(this.currentArmyInCountry == null) {
			this.currentArmyInCountry = new HashMap<>();
		}
		this.countriesHold.add(country);
		this.currentArmyInCountry.put(country, 0);
	}
	
	
	public void removeCountryHold(Country country) {
		if(this.countriesHold == null) {
			this.countriesHold = new ArrayList<>();
		}
		if(this.currentArmyInCountry == null) {
			this.currentArmyInCountry = new HashMap<>();
		}
		this.countriesHold.remove(country);
		this.currentArmyInCountry.remove(country);
	}
	
	
	
	//-------------------------------------------
	public void addCountry(Country country) {
		if(this.countriesHold == null) {
			this.countriesHold = new ArrayList<>();
		}
		
		this.countriesHold.add(country);
		country.setCountryOwner(this);
		d_can_get_card_this_turn = true;
	}
	
	public void removeCountry(Country country) {
		if(this.countriesHold == null) {
			this.countriesHold = new ArrayList<>();
		}
		
		this.countriesHold.remove(country);
	}
	
	public Country getCountry(Country p_country) {
		for(Country country : countriesHold) {
			if(country == p_country) {
				return country;
			}
		}
		return null;
	}
	
	public Player getPeaceWith() {
		return peaceWith;
	}
	
	public void setPeaceWith(Player player) {
		peaceWith = player;
	}
	
	public void resetPeaceWith() {
		peaceWith = null;
	}
	
	public List<Card> getCardList() {
		return d_cards;
	}
	
	public void addCard()
	{
		Card newCard = Card.generateRandomCard();
		
		if(d_cards.size() < MAX_CARD_LIMIT)
		{
			d_cards.add(newCard);
		}
		else
		{
			cardLimitExceeded(newCard);
		}
	}
	
	public void addSpecificCard(Card card) {
		
		if(d_cards.size() < MAX_CARD_LIMIT) {
			d_cards.add(card);
		}
		else {
			cardLimitExceeded(card);
		}
	}
	
	private void cardLimitExceeded(Card newCard) {
		System.out.println("Card limit that a player can possess has been exceeded, please choose the following options\n");
		
		System.out.println("The following cards are in your possession");
		System.out.println("------------------------------------------");
		
		for(int i = 0; i < d_cards.size(); ++i)
		{
			System.out.println(i+1 + ".	" + d_cards.get(i).getCardType().name());
		}
		
		System.out.println();
		System.out.println("NEW CARD");
		System.out.println("---------");
		
		System.out.println(newCard.getCardType().name()+"\n");
		
		
		int card_num;
		System.out.println("Please select which card you want to replace or PRESS 0 to keep current cards");
		
		while(true)
		{
			Scanner scan = new Scanner(System.in);
			
			
			if(scan.hasNextInt()) {
				card_num = scan.nextInt();
				
				if(card_num >= 0 && card_num <= MAX_CARD_LIMIT) {
					break;
				}
				else
				{
					System.out.println("Invalid input");
					
					for(int i = 0; i < d_cards.size(); ++i)
					{
						System.out.println("Select " + (i+1) + " to replace:	" + d_cards.get(i).getCardType().name());
					}
					
				}
			}
			else
			{
				System.out.println("Invalid input");
				
				for(int i = 0; i < d_cards.size(); ++i)
				{
					System.out.println("Select " + (i+1) + " to replace:	" + d_cards.get(i).getCardType().name());
				}
				
				
			}
		}
		
		if(card_num != 0)
		{
			d_cards.set(card_num - 1, newCard);
		}	
	}
	
	public void printCardList() {
		int card_num = 1;
		System.out.println("CARDS CURRENTLY OWNED BY: " + playerName);
		System.out.println("***********************************************************");
		
		for(Card card : d_cards) {
			System.out.println(card_num + ". " + card.getCardType().name());
			++card_num;
		}
		System.out.println("***********************************************************\n\n");
	}
	
	public boolean getCanAddCard() {
		return d_can_get_card_this_turn;
	}
	
	public void endTurnCardReset()
	{
		d_can_get_card_this_turn = false;
	}
	
	//-------------------------------------------
	
	
	
	public String getPlayerName() {
		return playerName;
	}


	public void setPlayerName(String playerName) {
		this.playerName = playerName;
	}

	public int getCurrentArmies() {
		return currentArmies;
	}


	public void setCurrentArmies(int currentArmies) {
		this.currentArmies = currentArmies;
	}

	public Deque<Order> getOrders() {
		return orders_list;
	}

	public void setOrders(Deque<Order> d_Orders) {
		this.orders_list = d_Orders;
	}

	public void addOrder(Order order) {
		if(orders_list == null) {
			this.orders_list = new ArrayDeque<>();
		}
		this.orders_list.add(order);
	}
	

	
}
