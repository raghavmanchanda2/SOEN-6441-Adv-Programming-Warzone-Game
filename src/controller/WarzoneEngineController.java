package controller;

import java.util.InputMismatchException;
import java.util.Scanner;

public class WarzoneEngineController {
	
	private Scanner inputForFeatureSelection;
	

	
	public WarzoneEngineController(){
		inputForFeatureSelection = new Scanner(System.in);
	}
	
	public int getGameFeatureInputs() {
		int selectedFeature;
		try {
			selectedFeature = inputForFeatureSelection.nextInt();
			
		
		}catch(InputMismatchException inputMismatchException){
			// throws global exception
			return -1;
		}
		
		if (selectedFeature > 0  && selectedFeature < 4)
		return selectedFeature;
		
		else
			return -1;
	}
	
	
}	
