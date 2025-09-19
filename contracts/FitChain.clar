;; FitChain: Decentralized Fitness Tracking and Wellness Reward System
;; Version: 1.0.0

;; Constants
(define-constant FITNESS_POOL_CAPACITY u2000000)
(define-constant BASE_WORKOUT_REWARD u26)
(define-constant CONSISTENCY_BONUS u10)
(define-constant MAX_FITNESS_LEVEL u14)
(define-constant ERR_INVALID_WORKOUT_DATA u1)
(define-constant ERR_NO_FITNESS_TOKENS u2)
(define-constant ERR_POOL_CAPACITY_EXCEEDED u3)
(define-constant BLOCKS_PER_FITNESS_CYCLE u1872)
(define-constant HEALTH_SAVINGS_MULTIPLIER u5)
(define-constant MIN_SAVINGS_PERIOD u936)
(define-constant EARLY_WITHDRAWAL_PENALTY u16)

;; Data Variables
(define-data-var total-fitness-tokens-distributed uint u0)
(define-data-var total-workouts-completed uint u0)
(define-data-var fitness-coordinator principal tx-sender)

;; Data Maps
(define-map athlete-workouts principal uint)
(define-map athlete-fitness-tokens principal uint)
(define-map workout-start-time principal uint)
(define-map athlete-fitness-level principal uint)
(define-map athlete-last-workout principal uint)
(define-map athlete-health-savings principal uint)
(define-map athlete-savings-start-block principal uint)
(define-map workout-type-specialty principal uint)
(define-map athlete-achievement-count principal uint)
(define-map training-expertise principal uint)

;; Public Functions
(define-public (start-workout-session (workout-duration uint) (exercise-type uint))
  (let
    (
      (athlete tx-sender)
    )
    (asserts! (and (> workout-duration u0) (> exercise-type u0) (<= exercise-type u15)) (err ERR_INVALID_WORKOUT_DATA))
    (map-set workout-start-time athlete burn-block-height)
    (map-set workout-type-specialty athlete exercise-type)
    (ok true)
  ))

(define-public (complete-workout-session (workout-duration uint) (intensity-level uint))
  (let
    (
      (athlete tx-sender)
      (start-block (default-to u0 (map-get? workout-start-time athlete)))
      (blocks-exercising (- burn-block-height start-block))
      (last-workout-block (default-to u0 (map-get? athlete-last-workout athlete)))
      (fitness-level (default-to u0 (map-get? athlete-fitness-level athlete)))
      (capped-fitness (if (<= fitness-level MAX_FITNESS_LEVEL) fitness-level MAX_FITNESS_LEVEL))
      (training-bonus (default-to u0 (map-get? training-expertise athlete)))
      (intensity-bonus (/ (* intensity-level u8) u100))
      (workout-reward (+ BASE_WORKOUT_REWARD (* capped-fitness CONSISTENCY_BONUS) training-bonus intensity-bonus))
    )
    (asserts! (and (> start-block u0) (>= blocks-exercising workout-duration) (<= intensity-level u100)) (err ERR_INVALID_WORKOUT_DATA))
    
    (map-set athlete-workouts athlete (+ (default-to u0 (map-get? athlete-workouts athlete)) u1))
    (map-set athlete-fitness-tokens athlete (+ (default-to u0 (map-get? athlete-fitness-tokens athlete)) workout-reward))
    
    (if (< (- burn-block-height last-workout-block) BLOCKS_PER_FITNESS_CYCLE)
      (map-set athlete-fitness-level athlete (+ fitness-level u1))
      (map-set athlete-fitness-level athlete u1)
    )
    
    (if (>= intensity-level u80)
      (begin
        (map-set athlete-achievement-count athlete (+ (default-to u0 (map-get? athlete-achievement-count athlete)) u1))
        (map-set training-expertise athlete (+ training-bonus u4))
      )
      true
    )
    
    (map-set athlete-last-workout athlete burn-block-height)
    (var-set total-workouts-completed (+ (var-get total-workouts-completed) u1))
    (var-set total-fitness-tokens-distributed (+ (var-get total-fitness-tokens-distributed) workout-reward))
    
    (asserts! (<= (var-get total-fitness-tokens-distributed) FITNESS_POOL_CAPACITY) (err ERR_POOL_CAPACITY_EXCEEDED))
    (ok workout-reward)
  ))

(define-public (claim-fitness-rewards)
  (let
    (
      (athlete tx-sender)
      (token-balance (default-to u0 (map-get? athlete-fitness-tokens athlete)))
    )
    (asserts! (> token-balance u0) (err ERR_NO_FITNESS_TOKENS))
    (map-set athlete-fitness-tokens athlete u0)
    (ok token-balance)
  ))

(define-public (save-for-health (amount uint))
  (let
    (
      (athlete tx-sender)
    )
    (asserts! (> amount u0) (err ERR_INVALID_WORKOUT_DATA))
    (asserts! (>= (var-get total-fitness-tokens-distributed) amount) (err ERR_POOL_CAPACITY_EXCEEDED))
    
    (map-set athlete-health-savings athlete amount)
    (map-set athlete-savings-start-block athlete burn-block-height)
    (var-set total-fitness-tokens-distributed (- (var-get total-fitness-tokens-distributed) amount))
    (ok amount)
  ))

(define-public (withdraw-health-savings)
  (let
    (
      (athlete tx-sender)
      (saved-amount (default-to u0 (map-get? athlete-health-savings athlete)))
      (savings-start-block (default-to u0 (map-get? athlete-savings-start-block athlete)))
      (blocks-saved (- burn-block-height savings-start-block))
      (penalty (if (< blocks-saved MIN_SAVINGS_PERIOD) (/ (* saved-amount EARLY_WITHDRAWAL_PENALTY) u100) u0))
      (health-bonus (if (>= blocks-saved MIN_SAVINGS_PERIOD) (/ (* saved-amount HEALTH_SAVINGS_MULTIPLIER) u100) u0))
      (final-amount (+ (- saved-amount penalty) health-bonus))
    )
    (asserts! (> saved-amount u0) (err ERR_NO_FITNESS_TOKENS))
    
    (map-set athlete-health-savings athlete u0)
    (map-set athlete-savings-start-block athlete u0)
    (var-set total-fitness-tokens-distributed (+ (var-get total-fitness-tokens-distributed) final-amount))
    (ok final-amount)
  ))

(define-public (establish-fitness-center (center-name (string-utf8 64)) (equipment-count uint))
  (let
    (
      (athlete tx-sender)
      (fitness-level (default-to u0 (map-get? athlete-fitness-level athlete)))
      (achievement-count (default-to u0 (map-get? athlete-achievement-count athlete)))
      (center-bonus (+ (* equipment-count u20) (* achievement-count u12) BASE_WORKOUT_REWARD))
    )
    (asserts! (and (> (len center-name) u0) (>= fitness-level u8) (> equipment-count u0)) (err ERR_INVALID_WORKOUT_DATA))
    
    (map-set athlete-fitness-tokens athlete (+ (default-to u0 (map-get? athlete-fitness-tokens athlete)) center-bonus))
    (var-set total-fitness-tokens-distributed (+ (var-get total-fitness-tokens-distributed) center-bonus))
    
    (ok center-bonus)
  ))

(define-public (conduct-fitness-training (trainee-count uint) (session-hours uint))
  (let
    (
      (athlete tx-sender)
      (fitness-level (default-to u0 (map-get? athlete-fitness-level athlete)))
      (training-expertise-level (default-to u0 (map-get? training-expertise athlete)))
      (training-bonus (+ (* trainee-count u18) (* session-hours u7) (* training-expertise-level u2)))
    )
    (asserts! (and (> trainee-count u0) (> session-hours u0) (>= fitness-level u10)) (err ERR_INVALID_WORKOUT_DATA))
    
    (map-set athlete-fitness-tokens athlete (+ (default-to u0 (map-get? athlete-fitness-tokens athlete)) training-bonus))
    (var-set total-fitness-tokens-distributed (+ (var-get total-fitness-tokens-distributed) training-bonus))
    
    (ok training-bonus)
  ))

;; Read-Only Functions
(define-read-only (get-workout-count (user principal))
  (default-to u0 (map-get? athlete-workouts user)))

(define-read-only (get-fitness-token-balance (user principal))
  (default-to u0 (map-get? athlete-fitness-tokens user)))

(define-read-only (get-fitness-level (user principal))
  (default-to u0 (map-get? athlete-fitness-level user)))

(define-read-only (get-achievement-count (user principal))
  (default-to u0 (map-get? athlete-achievement-count user)))

(define-read-only (get-health-savings (user principal))
  (default-to u0 (map-get? athlete-health-savings user)))

(define-read-only (get-training-expertise (user principal))
  (default-to u0 (map-get? training-expertise user)))

(define-read-only (get-fitness-stats)
  {
    total-workouts-completed: (var-get total-workouts-completed),
    total-fitness-tokens-distributed: (var-get total-fitness-tokens-distributed),
    fitness-pool-capacity: FITNESS_POOL_CAPACITY
  })

(define-read-only (calculate-workout-reward (fitness-level uint) (intensity-level uint) (training-bonus uint))
  (let
    (
      (capped-fitness (if (<= fitness-level MAX_FITNESS_LEVEL) fitness-level MAX_FITNESS_LEVEL))
      (intensity-bonus (/ (* intensity-level u8) u100))
    )
    (+ BASE_WORKOUT_REWARD (* capped-fitness CONSISTENCY_BONUS) training-bonus intensity-bonus)
  ))

;; Private Functions
(define-private (is-fitness-coordinator)
  (is-eq tx-sender (var-get fitness-coordinator)))

(define-private (validate-workout-parameters (workout-duration uint) (intensity-level uint))
  (and (> workout-duration u0) (<= intensity-level u100)))