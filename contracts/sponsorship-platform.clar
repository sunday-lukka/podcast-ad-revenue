;; Podcast Sponsorship Platform
;; A smart contract for podcast advertising revenue management with audience verification,
;; ad placement tracking, and revenue distribution for podcast creators.

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u401))
(define-constant ERR_NOT_FOUND (err u404))
(define-constant ERR_ALREADY_EXISTS (err u409))
(define-constant ERR_INSUFFICIENT_FUNDS (err u402))
(define-constant ERR_INVALID_AMOUNT (err u400))

;; data vars
(define-data-var next-podcast-id uint u1)
(define-data-var next-sponsor-id uint u1)
(define-data-var next-ad-placement-id uint u1)
(define-data-var platform-fee-percentage uint u5) ;; 5% platform fee

;; data maps
;; Podcast creators registry
(define-map podcasts
    { podcast-id: uint }
    {
        creator: principal,
        title: (string-ascii 100),
        verified-audience: uint,
        total-earnings: uint,
        active: bool
    }
)

;; Sponsors registry
(define-map sponsors
    { sponsor-id: uint }
    {
        company: principal,
        name: (string-ascii 50),
        budget: uint,
        spent: uint,
        active: bool
    }
)

;; Ad placements tracking
(define-map ad-placements
    { ad-id: uint }
    {
        podcast-id: uint,
        sponsor-id: uint,
        cost-per-impression: uint,
        total-impressions: uint,
        verified-impressions: uint,
        payment-amount: uint,
        status: (string-ascii 20), ;; "active", "completed", "cancelled"
        created-at: uint
    }
)

;; Podcast creator lookup by principal
(define-map creator-to-podcast { creator: principal } { podcast-id: uint })

;; Sponsor lookup by principal  
(define-map company-to-sponsor { company: principal } { sponsor-id: uint })

;; Revenue distribution tracking
(define-map revenue-distributions
    { podcast-id: uint, ad-id: uint }
    {
        creator-amount: uint,
        platform-amount: uint,
        distributed: bool,
        distribution-block: uint
    }
)

;; public functions

;; Register a podcast creator
(define-public (register-podcast (title (string-ascii 100)) (initial-audience uint))
    (let
        (
            (podcast-id (var-get next-podcast-id))
        )
        (asserts! (is-none (map-get? creator-to-podcast { creator: tx-sender })) ERR_ALREADY_EXISTS)
        (asserts! (> (len title) u0) ERR_INVALID_AMOUNT)
        
        ;; Store podcast data
        (map-set podcasts
            { podcast-id: podcast-id }
            {
                creator: tx-sender,
                title: title,
                verified-audience: initial-audience,
                total-earnings: u0,
                active: true
            }
        )
        
        ;; Store creator lookup
        (map-set creator-to-podcast 
            { creator: tx-sender } 
            { podcast-id: podcast-id }
        )
        
        ;; Increment next ID
        (var-set next-podcast-id (+ podcast-id u1))
        
        (ok podcast-id)
    )
)

;; Register a sponsor
(define-public (register-sponsor (company-name (string-ascii 50)) (initial-budget uint))
    (let
        (
            (sponsor-id (var-get next-sponsor-id))
        )
        (asserts! (is-none (map-get? company-to-sponsor { company: tx-sender })) ERR_ALREADY_EXISTS)
        (asserts! (> (len company-name) u0) ERR_INVALID_AMOUNT)
        (asserts! (> initial-budget u0) ERR_INVALID_AMOUNT)
        
        ;; Store sponsor data
        (map-set sponsors
            { sponsor-id: sponsor-id }
            {
                company: tx-sender,
                name: company-name,
                budget: initial-budget,
                spent: u0,
                active: true
            }
        )
        
        ;; Store company lookup
        (map-set company-to-sponsor 
            { company: tx-sender } 
            { sponsor-id: sponsor-id }
        )
        
        ;; Increment next ID
        (var-set next-sponsor-id (+ sponsor-id u1))
        
        (ok sponsor-id)
    )
)

;; Create an ad placement
(define-public (create-ad-placement (podcast-id uint) (cost-per-impression uint) (estimated-impressions uint))
    (let
        (
            (ad-id (var-get next-ad-placement-id))
            (sponsor-data (unwrap! (get-sponsor-by-principal tx-sender) ERR_UNAUTHORIZED))
            (sponsor-id (get sponsor-id sponsor-data))
            (sponsor-info (unwrap! (map-get? sponsors { sponsor-id: sponsor-id }) ERR_NOT_FOUND))
            (total-cost (* cost-per-impression estimated-impressions))
            (available-budget (- (get budget sponsor-info) (get spent sponsor-info)))
        )
        ;; Validate inputs
        (asserts! (is-some (map-get? podcasts { podcast-id: podcast-id })) ERR_NOT_FOUND)
        (asserts! (> cost-per-impression u0) ERR_INVALID_AMOUNT)
        (asserts! (> estimated-impressions u0) ERR_INVALID_AMOUNT)
        (asserts! (>= available-budget total-cost) ERR_INSUFFICIENT_FUNDS)
        (asserts! (get active sponsor-info) ERR_UNAUTHORIZED)
        
        ;; Create ad placement
        (map-set ad-placements
            { ad-id: ad-id }
            {
                podcast-id: podcast-id,
                sponsor-id: sponsor-id,
                cost-per-impression: cost-per-impression,
                total-impressions: estimated-impressions,
                verified-impressions: u0,
                payment-amount: u0,
                status: "active",
                created-at: stacks-block-height
            }
        )
        
        ;; Reserve budget
        (map-set sponsors
            { sponsor-id: sponsor-id }
            (merge sponsor-info { spent: (+ (get spent sponsor-info) total-cost) })
        )
        
        ;; Increment next ID
        (var-set next-ad-placement-id (+ ad-id u1))
        
        (ok ad-id)
    )
)

;; Update audience verification (only podcast creator can do this)
(define-public (update-audience-verification (podcast-id uint) (new-audience-count uint))
    (let
        (
            (podcast-data (unwrap! (map-get? podcasts { podcast-id: podcast-id }) ERR_NOT_FOUND))
        )
        ;; Only podcast creator can update
        (asserts! (is-eq tx-sender (get creator podcast-data)) ERR_UNAUTHORIZED)
        (asserts! (get active podcast-data) ERR_UNAUTHORIZED)
        
        ;; Update audience count
        (map-set podcasts
            { podcast-id: podcast-id }
            (merge podcast-data { verified-audience: new-audience-count })
        )
        
        (ok true)
    )
)

;; Record verified impressions and distribute revenue
(define-public (record-impressions (ad-id uint) (verified-impressions uint))
    (let
        (
            (ad-data (unwrap! (map-get? ad-placements { ad-id: ad-id }) ERR_NOT_FOUND))
            (podcast-data (unwrap! (map-get? podcasts { podcast-id: (get podcast-id ad-data) }) ERR_NOT_FOUND))
            (payment-amount (* (get cost-per-impression ad-data) verified-impressions))
            (platform-fee (/ (* payment-amount (var-get platform-fee-percentage)) u100))
            (creator-amount (- payment-amount platform-fee))
        )
        ;; Only podcast creator can record impressions
        (asserts! (is-eq tx-sender (get creator podcast-data)) ERR_UNAUTHORIZED)
        (asserts! (is-eq (get status ad-data) "active") ERR_UNAUTHORIZED)
        (asserts! (<= verified-impressions (get total-impressions ad-data)) ERR_INVALID_AMOUNT)
        
        ;; Update ad placement with verified impressions
        (map-set ad-placements
            { ad-id: ad-id }
            (merge ad-data { 
                verified-impressions: verified-impressions,
                payment-amount: payment-amount,
                status: "completed"
            })
        )
        
        ;; Update podcast earnings
        (map-set podcasts
            { podcast-id: (get podcast-id ad-data) }
            (merge podcast-data { 
                total-earnings: (+ (get total-earnings podcast-data) creator-amount)
            })
        )
        
        ;; Record revenue distribution
        (map-set revenue-distributions
            { podcast-id: (get podcast-id ad-data), ad-id: ad-id }
            {
                creator-amount: creator-amount,
                platform-amount: platform-fee,
                distributed: true,
                distribution-block: stacks-block-height
            }
        )
        
        (ok payment-amount)
    )
)

;; Add budget to sponsor account
(define-public (add-sponsor-budget (additional-budget uint))
    (let
        (
            (sponsor-data (unwrap! (get-sponsor-by-principal tx-sender) ERR_UNAUTHORIZED))
            (sponsor-id (get sponsor-id sponsor-data))
            (sponsor-info (unwrap! (map-get? sponsors { sponsor-id: sponsor-id }) ERR_NOT_FOUND))
        )
        (asserts! (> additional-budget u0) ERR_INVALID_AMOUNT)
        (asserts! (get active sponsor-info) ERR_UNAUTHORIZED)
        
        ;; Update sponsor budget
        (map-set sponsors
            { sponsor-id: sponsor-id }
            (merge sponsor-info { budget: (+ (get budget sponsor-info) additional-budget) })
        )
        
        (ok (+ (get budget sponsor-info) additional-budget))
    )
)

;; Update platform fee (only contract owner)
(define-public (update-platform-fee (new-fee-percentage uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (<= new-fee-percentage u20) ERR_INVALID_AMOUNT) ;; Max 20% fee
        
        (var-set platform-fee-percentage new-fee-percentage)
        (ok new-fee-percentage)
    )
)

;; read only functions

;; Get podcast information
(define-read-only (get-podcast (podcast-id uint))
    (map-get? podcasts { podcast-id: podcast-id })
)

;; Get sponsor information
(define-read-only (get-sponsor (sponsor-id uint))
    (map-get? sponsors { sponsor-id: sponsor-id })
)

;; Get ad placement information
(define-read-only (get-ad-placement (ad-id uint))
    (map-get? ad-placements { ad-id: ad-id })
)

;; Get podcast by creator principal
(define-read-only (get-podcast-by-creator (creator principal))
    (match (map-get? creator-to-podcast { creator: creator })
        podcast-ref (map-get? podcasts { podcast-id: (get podcast-id podcast-ref) })
        none
    )
)

;; Get sponsor by company principal
(define-read-only (get-sponsor-by-principal (company principal))
    (map-get? company-to-sponsor { company: company })
)

;; Get revenue distribution for an ad
(define-read-only (get-revenue-distribution (podcast-id uint) (ad-id uint))
    (map-get? revenue-distributions { podcast-id: podcast-id, ad-id: ad-id })
)

;; Get platform statistics
(define-read-only (get-platform-stats)
    {
        next-podcast-id: (var-get next-podcast-id),
        next-sponsor-id: (var-get next-sponsor-id),
        next-ad-placement-id: (var-get next-ad-placement-id),
        platform-fee-percentage: (var-get platform-fee-percentage)
    }
)

;; private functions
;; None needed for this implementation

