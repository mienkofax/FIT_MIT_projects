/**
  ******************************************************************************
  * File Name          : main.c
  * Description        : Main program body
  ******************************************************************************
  ** This notice applies to any and all portions of this file
  * that are not between comment pairs USER CODE BEGIN and
  * USER CODE END. Other portions of this file, whether 
  * inserted by the user or by software development tools
  * are owned by their respective copyright owners.
  *
  * COPYRIGHT(c) 2017 STMicroelectronics
  *
  * Redistribution and use in source and binary forms, with or without modification,
  * are permitted provided that the following conditions are met:
  *   1. Redistributions of source code must retain the above copyright notice,
  *      this list of conditions and the following disclaimer.
  *   2. Redistributions in binary form must reproduce the above copyright notice,
  *      this list of conditions and the following disclaimer in the documentation
  *      and/or other materials provided with the distribution.
  *   3. Neither the name of STMicroelectronics nor the names of its contributors
  *      may be used to endorse or promote products derived from this software
  *      without specific prior written permission.
  *
  * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
  * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  *
  ******************************************************************************
  */

/* Includes ------------------------------------------------------------------*/
#include "main.h"
#include "stm32l4xx_hal.h"

/* USER CODE BEGIN Includes */
#include "AdafruitFingerPrint.h"

/* USER CODE END Includes */

/* Private variables ---------------------------------------------------------*/
UART_HandleTypeDef huart1;

/* USER CODE BEGIN PV */
/* Private variables ---------------------------------------------------------*/

/* USER CODE END PV */

/* Private function prototypes -----------------------------------------------*/
void SystemClock_Config(void);
static void MX_GPIO_Init(void);
static void MX_USART1_UART_Init(void);

/* USER CODE BEGIN PFP */
/* Private function prototypes -----------------------------------------------*/

/* USER CODE END PFP */

/* USER CODE BEGIN 0 */

#define PUSH_BUTTON 0

HAL_StatusTypeDef readU(uint8_t *buffer, int len);
HAL_StatusTypeDef writeU(uint8_t *buffer, int len);

void fingerPrintEnroll();
void fingerPrintDelete();
void fingerPrintSearch();
void blinkRedLED(size_t n, int timeout);
void blinkBlueLED(size_t n, int timeout);

/* USER CODE END 0 */

int main(void)
{

  /* USER CODE BEGIN 1 */

  /* USER CODE END 1 */

  /* MCU Configuration----------------------------------------------------------*/

  /* Reset of all peripherals, Initializes the Flash interface and the Systick. */
  HAL_Init();

  /* USER CODE BEGIN Init */

  /* USER CODE END Init */

  /* Configure the system clock */
  SystemClock_Config();

  /* USER CODE BEGIN SysInit */

  /* USER CODE END SysInit */

  /* Initialize all configured peripherals */
  MX_GPIO_Init();
  MX_USART1_UART_Init();

  /* USER CODE BEGIN 2 */

  /* USER CODE END 2 */

  /* Infinite loop */
  /* USER CODE BEGIN WHILE */
  while (1)
  {
  /* USER CODE END WHILE */

  /* USER CODE BEGIN 3 */

	//check led status

	  HAL_GPIO_TogglePin(L_GREEN_GPIO_Port, L_GREEN_Pin);
	  HAL_Delay(150);
	  HAL_GPIO_TogglePin(L_RED_GPIO_Port, L_RED_Pin);
	  HAL_Delay(150);
	  HAL_GPIO_TogglePin(L_BLUE_GPIO_Port, L_BLUE_Pin);
	  HAL_Delay(150);

	  HAL_GPIO_TogglePin(L_GREEN_GPIO_Port, L_GREEN_Pin);
	  HAL_Delay(150);
	  HAL_GPIO_TogglePin(L_RED_GPIO_Port, L_RED_Pin);
	  HAL_Delay(150);
	  HAL_GPIO_TogglePin(L_BLUE_GPIO_Port, L_BLUE_Pin);
	  HAL_Delay(150);

	  while (1) {
		if (HAL_GPIO_ReadPin(BUTTON_3_GPIO_Port, BUTTON_3_Pin) == PUSH_BUTTON) {
	  		HAL_GPIO_TogglePin(L_GREEN_GPIO_Port, L_GREEN_Pin);
	  		fingerPrintEnroll();
	  		HAL_GPIO_TogglePin(L_GREEN_GPIO_Port, L_GREEN_Pin);
	  	}

		if (HAL_GPIO_ReadPin(BUTTON_2_GPIO_Port, BUTTON_2_Pin) == PUSH_BUTTON) {
			HAL_GPIO_TogglePin(L_GREEN_GPIO_Port, L_GREEN_Pin);
			fingerPrintSearch();
			HAL_GPIO_TogglePin(L_GREEN_GPIO_Port, L_GREEN_Pin);
		}
	  }
  }
  /* USER CODE END 3 */

}

/** System Clock Configuration
*/
void SystemClock_Config(void)
{

  RCC_OscInitTypeDef RCC_OscInitStruct;
  RCC_ClkInitTypeDef RCC_ClkInitStruct;
  RCC_PeriphCLKInitTypeDef PeriphClkInit;

    /**Initializes the CPU, AHB and APB busses clocks 
    */
  RCC_OscInitStruct.OscillatorType = RCC_OSCILLATORTYPE_MSI;
  RCC_OscInitStruct.MSIState = RCC_MSI_ON;
  RCC_OscInitStruct.MSICalibrationValue = 0;
  RCC_OscInitStruct.MSIClockRange = RCC_MSIRANGE_11;
  RCC_OscInitStruct.PLL.PLLState = RCC_PLL_NONE;
  if (HAL_RCC_OscConfig(&RCC_OscInitStruct) != HAL_OK)
  {
    _Error_Handler(__FILE__, __LINE__);
  }

    /**Initializes the CPU, AHB and APB busses clocks 
    */
  RCC_ClkInitStruct.ClockType = RCC_CLOCKTYPE_HCLK|RCC_CLOCKTYPE_SYSCLK
                              |RCC_CLOCKTYPE_PCLK1|RCC_CLOCKTYPE_PCLK2;
  RCC_ClkInitStruct.SYSCLKSource = RCC_SYSCLKSOURCE_MSI;
  RCC_ClkInitStruct.AHBCLKDivider = RCC_SYSCLK_DIV1;
  RCC_ClkInitStruct.APB1CLKDivider = RCC_HCLK_DIV1;
  RCC_ClkInitStruct.APB2CLKDivider = RCC_HCLK_DIV1;

  if (HAL_RCC_ClockConfig(&RCC_ClkInitStruct, FLASH_LATENCY_2) != HAL_OK)
  {
    _Error_Handler(__FILE__, __LINE__);
  }

  PeriphClkInit.PeriphClockSelection = RCC_PERIPHCLK_USART1;
  PeriphClkInit.Usart1ClockSelection = RCC_USART1CLKSOURCE_PCLK2;
  if (HAL_RCCEx_PeriphCLKConfig(&PeriphClkInit) != HAL_OK)
  {
    _Error_Handler(__FILE__, __LINE__);
  }

    /**Configure the main internal regulator output voltage 
    */
  if (HAL_PWREx_ControlVoltageScaling(PWR_REGULATOR_VOLTAGE_SCALE1) != HAL_OK)
  {
    _Error_Handler(__FILE__, __LINE__);
  }

    /**Configure the Systick interrupt time 
    */
  HAL_SYSTICK_Config(HAL_RCC_GetHCLKFreq()/1000);

    /**Configure the Systick 
    */
  HAL_SYSTICK_CLKSourceConfig(SYSTICK_CLKSOURCE_HCLK);

  /* SysTick_IRQn interrupt configuration */
  HAL_NVIC_SetPriority(SysTick_IRQn, 0, 0);
}

/* USART1 init function */
static void MX_USART1_UART_Init(void)
{

  huart1.Instance = USART1;
  huart1.Init.BaudRate = 57600;
  huart1.Init.WordLength = UART_WORDLENGTH_8B;
  huart1.Init.StopBits = UART_STOPBITS_1;
  huart1.Init.Parity = UART_PARITY_NONE;
  huart1.Init.Mode = UART_MODE_TX_RX;
  huart1.Init.HwFlowCtl = UART_HWCONTROL_NONE;
  huart1.Init.OverSampling = UART_OVERSAMPLING_16;
  huart1.Init.OneBitSampling = UART_ONE_BIT_SAMPLE_DISABLE;
  huart1.AdvancedInit.AdvFeatureInit = UART_ADVFEATURE_NO_INIT;
  if (HAL_UART_Init(&huart1) != HAL_OK)
  {
    _Error_Handler(__FILE__, __LINE__);
  }

}

/** Configure pins as 
        * Analog 
        * Input 
        * Output
        * EVENT_OUT
        * EXTI
*/
static void MX_GPIO_Init(void)
{

  GPIO_InitTypeDef GPIO_InitStruct;

  /* GPIO Ports Clock Enable */
  __HAL_RCC_GPIOC_CLK_ENABLE();
  __HAL_RCC_GPIOA_CLK_ENABLE();
  __HAL_RCC_GPIOB_CLK_ENABLE();

  /*Configure GPIO pin Output Level */
  HAL_GPIO_WritePin(GPIOA, L_GREEN_Pin|L_RED_Pin|L_BLUE_Pin, GPIO_PIN_RESET);

  /*Configure GPIO pin : USER_B1_Pin */
  GPIO_InitStruct.Pin = USER_B1_Pin;
  GPIO_InitStruct.Mode = GPIO_MODE_INPUT;
  GPIO_InitStruct.Pull = GPIO_PULLDOWN;
  HAL_GPIO_Init(USER_B1_GPIO_Port, &GPIO_InitStruct);

  /*Configure GPIO pins : L_GREEN_Pin L_RED_Pin */
  GPIO_InitStruct.Pin = L_GREEN_Pin|L_RED_Pin;
  GPIO_InitStruct.Mode = GPIO_MODE_OUTPUT_PP;
  GPIO_InitStruct.Pull = GPIO_NOPULL;
  GPIO_InitStruct.Speed = GPIO_SPEED_FREQ_LOW;
  HAL_GPIO_Init(GPIOA, &GPIO_InitStruct);

  /*Configure GPIO pin : L_BLUE_Pin */
  GPIO_InitStruct.Pin = L_BLUE_Pin;
  GPIO_InitStruct.Mode = GPIO_MODE_OUTPUT_PP;
  GPIO_InitStruct.Pull = GPIO_PULLUP;
  GPIO_InitStruct.Speed = GPIO_SPEED_FREQ_LOW;
  HAL_GPIO_Init(L_BLUE_GPIO_Port, &GPIO_InitStruct);

  /*Configure GPIO pins : BUTTON_2_Pin BUTTON_1_Pin */
  GPIO_InitStruct.Pin = BUTTON_2_Pin|BUTTON_1_Pin;
  GPIO_InitStruct.Mode = GPIO_MODE_INPUT;
  GPIO_InitStruct.Pull = GPIO_PULLUP;
  HAL_GPIO_Init(GPIOB, &GPIO_InitStruct);

  /*Configure GPIO pin : BUTTON_3_Pin */
  GPIO_InitStruct.Pin = BUTTON_3_Pin;
  GPIO_InitStruct.Mode = GPIO_MODE_INPUT;
  GPIO_InitStruct.Pull = GPIO_PULLUP;
  HAL_GPIO_Init(BUTTON_3_GPIO_Port, &GPIO_InitStruct);

}

/* USER CODE BEGIN 4 */
HAL_StatusTypeDef readU(uint8_t *buffer, int len)
{
	HAL_StatusTypeDef status = HAL_TIMEOUT;
	status = HAL_UART_Receive(&huart1, (uint8_t *) buffer, len, 10000);

	return status;
}

HAL_StatusTypeDef writeU(uint8_t *buffer, int len)
{
	return HAL_UART_Transmit(&huart1, buffer, len, HAL_MAX_DELAY);
}

void fingerPrintEnroll()
{
	struct AdafruitConfig conf = {0xffffffff, 0};
	struct AdafruitPacket sentPacket = {{0}, 0};
	struct AdafruitPacket receivePacket = {{0}, 0};
	struct AdafruitPayload payload = {{0}, 0};

	int ret = -1;

	//verify password
	sentPacket = verifyPassword(&conf);
	writeU(sentPacket.data, sentPacket.len);
	receivePacket.len = 12;
	readU(receivePacket.data, 12);
	ret = parsePacket(&receivePacket, &payload);

	if (ret != PACKET_OK)
		blinkRedLED(2, 150);
	else
		blinkBlueLED(1, 150);

	//read image
	while (1) {
		sentPacket = readImage(&conf);
		writeU(sentPacket.data, sentPacket.len);
		receivePacket.len = 12;
		readU(receivePacket.data, 12);
		ret = parsePacket(&receivePacket, &payload);

		if (ret != PACKET_OK || payload.data[1] != 0) {
			blinkRedLED(1, 150);
		}
		else {
			blinkBlueLED(1, 150);
			break;
		}
	}

	//convert image
	sentPacket = convertImage(&conf, BUFFER_1);
	writeU(sentPacket.data, sentPacket.len);
	receivePacket.len = 12;
	readU(receivePacket.data, 12);
	ret = parsePacket(&receivePacket, &payload);

	if (ret != PACKET_OK)
		blinkRedLED(1, 150);
	else
		blinkBlueLED(1, 150);

/*	//read image2
	while (1) {
		sentPacket = readImage(&conf);
		writeU(sentPacket.data, sentPacket.len);
		receivePacket.len = 12;
		readU(receivePacket.data, 12);
		ret = parsePacket(&receivePacket, &payload);

		if (ret != PACKET_OK || payload.data[1] != 0) {
			blinkRedLED(1, 150);
		}
		else {
			blinkBlueLED(1, 150);
			break;
		}
	}

	//convert image2
	sentPacket = convertImage(&conf, BUFFER_2);
	writeU(sentPacket.data, sentPacket.len);
	receivePacket.len = 12;
	readU(receivePacket.data, 12);
	ret = parsePacket(&receivePacket, &payload);

	if (ret != PACKET_OK)
		blinkRedLED(1, 150);
	else
		blinkBlueLED(1, 150);
*/
	//create model
	sentPacket = createModel(&conf);
	writeU(sentPacket.data, sentPacket.len);
	receivePacket.len = 12;
	readU(receivePacket.data, 12);
	ret = parsePacket(&receivePacket, &payload);

	if (ret != PACKET_OK)
		blinkRedLED(1, 150);
	else
		blinkBlueLED(1, 150);

	//store model
	sentPacket = storeModel(&conf, BUFFER_1, 1);
	writeU(sentPacket.data, sentPacket.len);
	receivePacket.len = 12;
	readU(receivePacket.data, 12);
	ret = parsePacket(&receivePacket, &payload);

	if (ret != PACKET_OK)
		blinkRedLED(1, 150);
	else
		blinkBlueLED(1, 150);
}

void fingerPrintDelete()
{

}

void fingerPrintSearch()
{
	struct AdafruitConfig conf = {0xffffffff, 0};
	struct AdafruitPacket sentPacket = {{0}, 0};
	struct AdafruitPacket receivePacket = {{0}, 0};
	struct AdafruitPayload payload = {{0}, 0};

	int ret = -1;

	//verify password
	sentPacket = verifyPassword(&conf);
	writeU(sentPacket.data, sentPacket.len);
	receivePacket.len = 12;
	readU(receivePacket.data, 12);
	ret = parsePacket(&receivePacket, &payload);

	if (ret != PACKET_OK)
		blinkRedLED(1, 150);
	else
		blinkBlueLED(1, 150);

	//read image
	while (1) {
		sentPacket = readImage(&conf);
		writeU(sentPacket.data, sentPacket.len);
		receivePacket.len = 12;
		readU(receivePacket.data, 12);
		ret = parsePacket(&receivePacket, &payload);

		if (ret != PACKET_OK || payload.data[1] != 0) {
			blinkRedLED(1, 150);
		}
		else {
			blinkBlueLED(1, 150);
			break;
		}
	}

	//convert image
	sentPacket = convertImage(&conf, BUFFER_1);
	writeU(sentPacket.data, sentPacket.len);
	receivePacket.len = 12;
	readU(receivePacket.data, 12);
	ret = parsePacket(&receivePacket, &payload);

	if (ret != PACKET_OK)
		blinkRedLED(1, 150);
	else
		blinkBlueLED(1, 150);

	// search
	sentPacket = fastSearch(&conf);
	writeU(sentPacket.data, sentPacket.len);
	receivePacket.len = 16;
	readU(receivePacket.data, 16);
	ret = parsePacket(&receivePacket, &payload);

	if (ret != PACKET_OK || payload.data[1] != 0)
		blinkRedLED(1, 2000);
	else {
		blinkBlueLED(1, 2000);
	}
}

void blinkRedLED(size_t n, int timeout)
{
	for (size_t i = 0; i < n; i++) {
		HAL_GPIO_TogglePin(L_RED_GPIO_Port, L_RED_Pin);
		HAL_Delay(timeout);
		HAL_GPIO_TogglePin(L_RED_GPIO_Port, L_RED_Pin);
		HAL_Delay(timeout);
	}
}

void blinkBlueLED(size_t n, int timeout)
{
	for (size_t i = 0; i < n; i++) {
		HAL_GPIO_TogglePin(L_BLUE_GPIO_Port, L_BLUE_Pin);
		HAL_Delay(timeout);
		HAL_GPIO_TogglePin(L_BLUE_GPIO_Port, L_BLUE_Pin);
		HAL_Delay(timeout);
	}
}

/* USER CODE END 4 */

/**
  * @brief  This function is executed in case of error occurrence.
  * @param  None
  * @retval None
  */
void _Error_Handler(char * file, int line)
{
  /* USER CODE BEGIN Error_Handler_Debug */
  /* User can add his own implementation to report the HAL error return state */
  while(1) 
  {
  }
  /* USER CODE END Error_Handler_Debug */ 
}

#ifdef USE_FULL_ASSERT

/**
   * @brief Reports the name of the source file and the source line number
   * where the assert_param error has occurred.
   * @param file: pointer to the source file name
   * @param line: assert_param error line source number
   * @retval None
   */
void assert_failed(uint8_t* file, uint32_t line)
{
  /* USER CODE BEGIN 6 */
  /* User can add his own implementation to report the file name and line number,
    ex: printf("Wrong parameters value: file %s on line %d\r\n", file, line) */
  /* USER CODE END 6 */

}

#endif

/**
  * @}
  */ 

/**
  * @}
*/ 

/************************ (C) COPYRIGHT STMicroelectronics *****END OF FILE****/
