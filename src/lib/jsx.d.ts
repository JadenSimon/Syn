/// <reference no-default-lib="true"/>

/// <reference lib="dom" />

// TODO: don't leak these types into ambient scope

/** [MDN Reference](https://developer.mozilla.org/docs/Web/API/ToggleEvent) */
interface ToggleEvent extends Event {
	readonly newState: string;
	readonly oldState: string;
}

declare var ToggleEvent: {
	prototype: ToggleEvent;
	new (type: string, eventInitDict?: ToggleEventInit): ToggleEvent;
};

interface ToggleEventInit extends EventInit {
	newState?: string;
	oldState?: string;
}

/** [MDN Reference](https://developer.mozilla.org/en-US/docs/Web/API/CommandEvent) */
interface CommandEvent extends Event {
	readonly source: Element | null;
	readonly command: string;
}

declare var CommandEvent: {
	prototype: CommandEvent;
	new (type: string, eventInitDict?: CommandEventInit): CommandEvent;
};

interface CommandEventInit extends EventInit {
	source: Element | null;
	command: string;
}

/** [MDN Reference](https://developer.mozilla.org/en-US/docs/Web/API/SnapEvent) */
interface SnapEvent extends Event {
	readonly snapTargetBlock: Element | null;
	readonly snapTargetInline: Element | null;
}

declare var SnapEvent: {
	prototype: SnapEvent;
	new (type: string, eventInitDict?: SnapEventInit): SnapEvent;
}

interface SnapEventInit extends EventInit {
	snapTargetBlock?: Element | null;
	snapTargetInline?: Element | null;
}

declare namespace JSX {
	type Booleanish = boolean | 'true' | 'false';

	type Child = Element | string | number | boolean | null | undefined;
	type Children = Child | Child[];

	type Element = HTMLElement;

	// ============================================
	// CSS Properties
	// ============================================

	type DOMCSSProperties = {
		[key in keyof Omit<
			CSSStyleDeclaration,
			| 'item'
			| 'setProperty'
			| 'removeProperty'
			| 'getPropertyValue'
			| 'getPropertyPriority'
		>]?: string | number | null;
	};

	interface CSSProperties extends DOMCSSProperties {
		cssText?: string | null;
		[key: string]: string | number | null | undefined;
	}

	// ============================================
	// Event Types
	// ============================================

	type TargetedEvent<
		Target extends EventTarget = EventTarget,
		TypedEvent extends Event = Event
	> = Omit<TypedEvent, 'currentTarget'> & {
		readonly currentTarget: Target;
	};

	type EventHandler<E extends TargetedEvent> = {
		bivarianceHack(event: E): void;
	}['bivarianceHack'];

	type AnimationEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, AnimationEvent>>;
	type ClipboardEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, ClipboardEvent>>;
	type CommandEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, CommandEvent>>;
	type CompositionEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, CompositionEvent>>;
	type DragEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, DragEvent>>;
	type ToggleEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, ToggleEvent>>;
	type FocusEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, FocusEvent>>;
	type GenericEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, Event>>;
	type InputEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, InputEvent>>;
	type KeyboardEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, KeyboardEvent>>;
	type MouseEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, MouseEvent>>;
	type PointerEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, PointerEvent>>;
	type SnapEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, SnapEvent>>;
	type SubmitEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, SubmitEvent>>;
	type TouchEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, TouchEvent>>;
	type TransitionEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, TransitionEvent>>;
	type UIEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, UIEvent>>;
	type WheelEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, WheelEvent>>;
	type PictureInPictureEventHandler<Target extends EventTarget> =
		EventHandler<TargetedEvent<Target, PictureInPictureEvent>>;

	// ============================================
	// DOM Attributes (Event Handlers)
	// ============================================

	interface DOMAttributes<Target extends EventTarget> {
		children?: Children;

		// Image Events
		onLoad?: GenericEventHandler<Target>;
		onLoadCapture?: GenericEventHandler<Target>;
		onError?: GenericEventHandler<Target>;
		onErrorCapture?: GenericEventHandler<Target>;

		// Clipboard Events
		onCopy?: ClipboardEventHandler<Target>;
		onCopyCapture?: ClipboardEventHandler<Target>;
		onCut?: ClipboardEventHandler<Target>;
		onCutCapture?: ClipboardEventHandler<Target>;
		onPaste?: ClipboardEventHandler<Target>;
		onPasteCapture?: ClipboardEventHandler<Target>;

		// Composition Events
		onCompositionEnd?: CompositionEventHandler<Target>;
		onCompositionEndCapture?: CompositionEventHandler<Target>;
		onCompositionStart?: CompositionEventHandler<Target>;
		onCompositionStartCapture?: CompositionEventHandler<Target>;
		onCompositionUpdate?: CompositionEventHandler<Target>;
		onCompositionUpdateCapture?: CompositionEventHandler<Target>;

		// Popover Events
		onBeforeToggle?: ToggleEventHandler<Target>;
		onToggle?: ToggleEventHandler<Target>;

		// Dialog Events
		onClose?: GenericEventHandler<Target>;
		onCancel?: GenericEventHandler<Target>;

		// Focus Events
		onFocus?: FocusEventHandler<Target>;
		onFocusCapture?: FocusEventHandler<Target>;
		onFocusIn?: FocusEventHandler<Target>;
		onFocusInCapture?: FocusEventHandler<Target>;
		onFocusOut?: FocusEventHandler<Target>;
		onFocusOutCapture?: FocusEventHandler<Target>;
		onBlur?: FocusEventHandler<Target>;
		onBlurCapture?: FocusEventHandler<Target>;

		// Form Events
		onChange?: GenericEventHandler<Target>;
		onChangeCapture?: GenericEventHandler<Target>;
		onInput?: InputEventHandler<Target>;
		onInputCapture?: InputEventHandler<Target>;
		onBeforeInput?: InputEventHandler<Target>;
		onBeforeInputCapture?: InputEventHandler<Target>;
		onSearch?: GenericEventHandler<Target>;
		onSearchCapture?: GenericEventHandler<Target>;
		onSubmit?: SubmitEventHandler<Target>;
		onSubmitCapture?: SubmitEventHandler<Target>;
		onInvalid?: GenericEventHandler<Target>;
		onInvalidCapture?: GenericEventHandler<Target>;
		onReset?: GenericEventHandler<Target>;
		onResetCapture?: GenericEventHandler<Target>;
		onFormData?: GenericEventHandler<Target>;
		onFormDataCapture?: GenericEventHandler<Target>;

		// Keyboard Events
		onKeyDown?: KeyboardEventHandler<Target>;
		onKeyDownCapture?: KeyboardEventHandler<Target>;
		onKeyPress?: KeyboardEventHandler<Target>;
		onKeyPressCapture?: KeyboardEventHandler<Target>;
		onKeyUp?: KeyboardEventHandler<Target>;
		onKeyUpCapture?: KeyboardEventHandler<Target>;

		// Media Events
		onAbort?: GenericEventHandler<Target>;
		onAbortCapture?: GenericEventHandler<Target>;
		onCanPlay?: GenericEventHandler<Target>;
		onCanPlayCapture?: GenericEventHandler<Target>;
		onCanPlayThrough?: GenericEventHandler<Target>;
		onCanPlayThroughCapture?: GenericEventHandler<Target>;
		onDurationChange?: GenericEventHandler<Target>;
		onDurationChangeCapture?: GenericEventHandler<Target>;
		onEmptied?: GenericEventHandler<Target>;
		onEmptiedCapture?: GenericEventHandler<Target>;
		onEncrypted?: GenericEventHandler<Target>;
		onEncryptedCapture?: GenericEventHandler<Target>;
		onEnded?: GenericEventHandler<Target>;
		onEndedCapture?: GenericEventHandler<Target>;
		onLoadedData?: GenericEventHandler<Target>;
		onLoadedDataCapture?: GenericEventHandler<Target>;
		onLoadedMetadata?: GenericEventHandler<Target>;
		onLoadedMetadataCapture?: GenericEventHandler<Target>;
		onLoadStart?: GenericEventHandler<Target>;
		onLoadStartCapture?: GenericEventHandler<Target>;
		onPause?: GenericEventHandler<Target>;
		onPauseCapture?: GenericEventHandler<Target>;
		onPlay?: GenericEventHandler<Target>;
		onPlayCapture?: GenericEventHandler<Target>;
		onPlaying?: GenericEventHandler<Target>;
		onPlayingCapture?: GenericEventHandler<Target>;
		onProgress?: GenericEventHandler<Target>;
		onProgressCapture?: GenericEventHandler<Target>;
		onRateChange?: GenericEventHandler<Target>;
		onRateChangeCapture?: GenericEventHandler<Target>;
		onSeeked?: GenericEventHandler<Target>;
		onSeekedCapture?: GenericEventHandler<Target>;
		onSeeking?: GenericEventHandler<Target>;
		onSeekingCapture?: GenericEventHandler<Target>;
		onStalled?: GenericEventHandler<Target>;
		onStalledCapture?: GenericEventHandler<Target>;
		onSuspend?: GenericEventHandler<Target>;
		onSuspendCapture?: GenericEventHandler<Target>;
		onTimeUpdate?: GenericEventHandler<Target>;
		onTimeUpdateCapture?: GenericEventHandler<Target>;
		onVolumeChange?: GenericEventHandler<Target>;
		onVolumeChangeCapture?: GenericEventHandler<Target>;
		onWaiting?: GenericEventHandler<Target>;
		onWaitingCapture?: GenericEventHandler<Target>;

		// MouseEvents
		onClick?: MouseEventHandler<Target>;
		onClickCapture?: MouseEventHandler<Target>;
		onContextMenu?: MouseEventHandler<Target>;
		onContextMenuCapture?: MouseEventHandler<Target>;
		onDblClick?: MouseEventHandler<Target>;
		onDblClickCapture?: MouseEventHandler<Target>;
		onDrag?: DragEventHandler<Target>;
		onDragCapture?: DragEventHandler<Target>;
		onDragEnd?: DragEventHandler<Target>;
		onDragEndCapture?: DragEventHandler<Target>;
		onDragEnter?: DragEventHandler<Target>;
		onDragEnterCapture?: DragEventHandler<Target>;
		onDragExit?: DragEventHandler<Target>;
		onDragExitCapture?: DragEventHandler<Target>;
		onDragLeave?: DragEventHandler<Target>;
		onDragLeaveCapture?: DragEventHandler<Target>;
		onDragOver?: DragEventHandler<Target>;
		onDragOverCapture?: DragEventHandler<Target>;
		onDragStart?: DragEventHandler<Target>;
		onDragStartCapture?: DragEventHandler<Target>;
		onDrop?: DragEventHandler<Target>;
		onDropCapture?: DragEventHandler<Target>;
		onMouseDown?: MouseEventHandler<Target>;
		onMouseDownCapture?: MouseEventHandler<Target>;
		onMouseEnter?: MouseEventHandler<Target>;
		onMouseEnterCapture?: MouseEventHandler<Target>;
		onMouseLeave?: MouseEventHandler<Target>;
		onMouseLeaveCapture?: MouseEventHandler<Target>;
		onMouseMove?: MouseEventHandler<Target>;
		onMouseMoveCapture?: MouseEventHandler<Target>;
		onMouseOut?: MouseEventHandler<Target>;
		onMouseOutCapture?: MouseEventHandler<Target>;
		onMouseOver?: MouseEventHandler<Target>;
		onMouseOverCapture?: MouseEventHandler<Target>;
		onMouseUp?: MouseEventHandler<Target>;
		onMouseUpCapture?: MouseEventHandler<Target>;
		onAuxClick?: MouseEventHandler<Target>;
		onAuxClickCapture?: MouseEventHandler<Target>;

		// Selection Events
		onSelect?: GenericEventHandler<Target>;
		onSelectCapture?: GenericEventHandler<Target>;

		// Touch Events
		onTouchCancel?: TouchEventHandler<Target>;
		onTouchCancelCapture?: TouchEventHandler<Target>;
		onTouchEnd?: TouchEventHandler<Target>;
		onTouchEndCapture?: TouchEventHandler<Target>;
		onTouchMove?: TouchEventHandler<Target>;
		onTouchMoveCapture?: TouchEventHandler<Target>;
		onTouchStart?: TouchEventHandler<Target>;
		onTouchStartCapture?: TouchEventHandler<Target>;

		// Pointer Events
		onPointerOver?: PointerEventHandler<Target>;
		onPointerOverCapture?: PointerEventHandler<Target>;
		onPointerEnter?: PointerEventHandler<Target>;
		onPointerEnterCapture?: PointerEventHandler<Target>;
		onPointerDown?: PointerEventHandler<Target>;
		onPointerDownCapture?: PointerEventHandler<Target>;
		onPointerMove?: PointerEventHandler<Target>;
		onPointerMoveCapture?: PointerEventHandler<Target>;
		onPointerUp?: PointerEventHandler<Target>;
		onPointerUpCapture?: PointerEventHandler<Target>;
		onPointerCancel?: PointerEventHandler<Target>;
		onPointerCancelCapture?: PointerEventHandler<Target>;
		onPointerOut?: PointerEventHandler<Target>;
		onPointerOutCapture?: PointerEventHandler<Target>;
		onPointerLeave?: PointerEventHandler<Target>;
		onPointerLeaveCapture?: PointerEventHandler<Target>;
		onGotPointerCapture?: PointerEventHandler<Target>;
		onGotPointerCaptureCapture?: PointerEventHandler<Target>;
		onLostPointerCapture?: PointerEventHandler<Target>;
		onLostPointerCaptureCapture?: PointerEventHandler<Target>;

		// Scroll Events
		onScroll?: GenericEventHandler<Target>;
		onScrollCapture?: GenericEventHandler<Target>;
		onScrollEnd?: GenericEventHandler<Target>;
		onScrollEndCapture?: GenericEventHandler<Target>;
		onScrollSnapChange?: SnapEventHandler<Target>;
		onScrollSnapChangeCapture?: SnapEventHandler<Target>;
		onScrollSnapChanging?: SnapEventHandler<Target>;
		onScrollSnapChangingCapture?: SnapEventHandler<Target>;

		// Wheel Events
		onWheel?: WheelEventHandler<Target>;
		onWheelCapture?: WheelEventHandler<Target>;

		// Animation Events
		onAnimationStart?: AnimationEventHandler<Target>;
		onAnimationStartCapture?: AnimationEventHandler<Target>;
		onAnimationEnd?: AnimationEventHandler<Target>;
		onAnimationEndCapture?: AnimationEventHandler<Target>;
		onAnimationIteration?: AnimationEventHandler<Target>;
		onAnimationIterationCapture?: AnimationEventHandler<Target>;

		// Transition Events
		onTransitionCancel?: TransitionEventHandler<Target>;
		onTransitionCancelCapture?: TransitionEventHandler<Target>;
		onTransitionEnd?: TransitionEventHandler<Target>;
		onTransitionEndCapture?: TransitionEventHandler<Target>;
		onTransitionRun?: TransitionEventHandler<Target>;
		onTransitionRunCapture?: TransitionEventHandler<Target>;
		onTransitionStart?: TransitionEventHandler<Target>;
		onTransitionStartCapture?: TransitionEventHandler<Target>;

		// PictureInPicture Events
		onEnterPictureInPicture?: PictureInPictureEventHandler<Target>;
		onEnterPictureInPictureCapture?: PictureInPictureEventHandler<Target>;
		onLeavePictureInPicture?: PictureInPictureEventHandler<Target>;
		onLeavePictureInPictureCapture?: PictureInPictureEventHandler<Target>;
		onResize?: PictureInPictureEventHandler<Target>;
		onResizeCapture?: PictureInPictureEventHandler<Target>;

		onCommand?: CommandEventHandler<Target>;
	}

	// ============================================
	// ARIA Attributes
	// ============================================

	interface AriaAttributes {
		'aria-activedescendant'?: string;
		'aria-atomic'?: Booleanish;
		'aria-autocomplete'?: 'none' | 'inline' | 'list' | 'both';
		'aria-braillelabel'?: string;
		'aria-brailleroledescription'?: string;
		'aria-busy'?: Booleanish;
		'aria-checked'?: Booleanish | 'mixed';
		'aria-colcount'?: number;
		'aria-colindex'?: number;
		'aria-colindextext'?: string;
		'aria-colspan'?: number;
		'aria-controls'?: string;
		'aria-current'?: Booleanish | 'page' | 'step' | 'location' | 'date' | 'time';
		'aria-describedby'?: string;
		'aria-description'?: string;
		'aria-details'?: string;
		'aria-disabled'?: Booleanish;
		'aria-dropeffect'?: 'none' | 'copy' | 'execute' | 'link' | 'move' | 'popup';
		'aria-errormessage'?: string;
		'aria-expanded'?: Booleanish;
		'aria-flowto'?: string;
		'aria-grabbed'?: Booleanish;
		'aria-haspopup'?: Booleanish | 'menu' | 'listbox' | 'tree' | 'grid' | 'dialog';
		'aria-hidden'?: Booleanish;
		'aria-invalid'?: Booleanish | 'grammar' | 'spelling';
		'aria-keyshortcuts'?: string;
		'aria-label'?: string;
		'aria-labelledby'?: string;
		'aria-level'?: number;
		'aria-live'?: 'off' | 'assertive' | 'polite';
		'aria-modal'?: Booleanish;
		'aria-multiline'?: Booleanish;
		'aria-multiselectable'?: Booleanish;
		'aria-orientation'?: 'horizontal' | 'vertical';
		'aria-owns'?: string;
		'aria-placeholder'?: string;
		'aria-posinset'?: number;
		'aria-pressed'?: Booleanish | 'mixed';
		'aria-readonly'?: Booleanish;
		'aria-relevant'?:
			| 'additions'
			| 'additions removals'
			| 'additions text'
			| 'all'
			| 'removals'
			| 'removals additions'
			| 'removals text'
			| 'text'
			| 'text additions'
			| 'text removals';
		'aria-required'?: Booleanish;
		'aria-roledescription'?: string;
		'aria-rowcount'?: number;
		'aria-rowindex'?: number;
		'aria-rowindextext'?: string;
		'aria-rowspan'?: number;
		'aria-selected'?: Booleanish;
		'aria-setsize'?: number;
		'aria-sort'?: 'none' | 'ascending' | 'descending' | 'other';
		'aria-valuemax'?: number;
		'aria-valuemin'?: number;
		'aria-valuenow'?: number;
		'aria-valuetext'?: string;
	}

	// WAI-ARIA 1.2 role attribute values
	type WAIAriaRole =
		| 'alert'
		| 'alertdialog'
		| 'application'
		| 'article'
		| 'banner'
		| 'blockquote'
		| 'button'
		| 'caption'
		| 'cell'
		| 'checkbox'
		| 'code'
		| 'columnheader'
		| 'combobox'
		| 'command'
		| 'complementary'
		| 'composite'
		| 'contentinfo'
		| 'definition'
		| 'deletion'
		| 'dialog'
		| 'directory'
		| 'document'
		| 'emphasis'
		| 'feed'
		| 'figure'
		| 'form'
		| 'grid'
		| 'gridcell'
		| 'group'
		| 'heading'
		| 'img'
		| 'input'
		| 'insertion'
		| 'landmark'
		| 'link'
		| 'list'
		| 'listbox'
		| 'listitem'
		| 'log'
		| 'main'
		| 'marquee'
		| 'math'
		| 'meter'
		| 'menu'
		| 'menubar'
		| 'menuitem'
		| 'menuitemcheckbox'
		| 'menuitemradio'
		| 'navigation'
		| 'none'
		| 'note'
		| 'option'
		| 'paragraph'
		| 'presentation'
		| 'progressbar'
		| 'radio'
		| 'radiogroup'
		| 'range'
		| 'region'
		| 'roletype'
		| 'row'
		| 'rowgroup'
		| 'rowheader'
		| 'scrollbar'
		| 'search'
		| 'searchbox'
		| 'section'
		| 'sectionhead'
		| 'select'
		| 'separator'
		| 'slider'
		| 'spinbutton'
		| 'status'
		| 'strong'
		| 'structure'
		| 'subscript'
		| 'superscript'
		| 'switch'
		| 'tab'
		| 'table'
		| 'tablist'
		| 'tabpanel'
		| 'term'
		| 'textbox'
		| 'time'
		| 'timer'
		| 'toolbar'
		| 'tooltip'
		| 'tree'
		| 'treegrid'
		| 'treeitem'
		| 'widget'
		| 'window'
		| 'none presentation';

	type DPubAriaRole =
		| 'doc-abstract'
		| 'doc-acknowledgments'
		| 'doc-afterword'
		| 'doc-appendix'
		| 'doc-backlink'
		| 'doc-biblioentry'
		| 'doc-bibliography'
		| 'doc-biblioref'
		| 'doc-chapter'
		| 'doc-colophon'
		| 'doc-conclusion'
		| 'doc-cover'
		| 'doc-credit'
		| 'doc-credits'
		| 'doc-dedication'
		| 'doc-endnote'
		| 'doc-endnotes'
		| 'doc-epigraph'
		| 'doc-epilogue'
		| 'doc-errata'
		| 'doc-example'
		| 'doc-footnote'
		| 'doc-foreword'
		| 'doc-glossary'
		| 'doc-glossref'
		| 'doc-index'
		| 'doc-introduction'
		| 'doc-noteref'
		| 'doc-notice'
		| 'doc-pagebreak'
		| 'doc-pagelist'
		| 'doc-part'
		| 'doc-preface'
		| 'doc-prologue'
		| 'doc-pullquote'
		| 'doc-qna'
		| 'doc-subtitle'
		| 'doc-tip'
		| 'doc-toc';

	type AriaRole = WAIAriaRole | DPubAriaRole;

	// ============================================
	// HTML Attributes
	// ============================================

	interface HTMLAttributes<RefType extends EventTarget = EventTarget>
		extends DOMAttributes<RefType>,
			AriaAttributes {
		// Standard HTML Attributes
		accesskey?: string;
		accessKey?: string;
		autocapitalize?: 'off' | 'none' | 'on' | 'sentences' | 'words' | 'characters';
		autoCapitalize?: 'off' | 'none' | 'on' | 'sentences' | 'words' | 'characters';
		autocorrect?: string;
		autoCorrect?: string;
		autofocus?: boolean;
		autoFocus?: boolean;
		class?: string;
		className?: string;
		contenteditable?: Booleanish | '' | 'plaintext-only' | 'inherit';
		contentEditable?: Booleanish | '' | 'plaintext-only' | 'inherit';
		dir?: 'auto' | 'rtl' | 'ltr';
		draggable?: boolean;
		enterkeyhint?: 'enter' | 'done' | 'go' | 'next' | 'previous' | 'search' | 'send';
		exportparts?: string;
		hidden?: boolean | 'hidden' | 'until-found';
		id?: string;
		inert?: boolean;
		inputmode?: string;
		inputMode?: string;
		is?: string;
		lang?: string;
		nonce?: string;
		part?: string;
		popover?: 'auto' | 'hint' | 'manual' | boolean;
		slot?: string;
		spellcheck?: boolean;
		style?: string | CSSProperties;
		tabindex?: number;
		tabIndex?: number;
		title?: string;
		translate?: boolean;

		// WAI-ARIA Attributes
		role?: AriaRole;

		// Non-standard Attributes
		disablePictureInPicture?: boolean;
		elementtiming?: string;
		elementTiming?: string;
		results?: number;

		// RDFa Attributes
		about?: string;
		datatype?: string;
		inlist?: any;
		prefix?: string;
		property?: string;
		resource?: string;
		typeof?: string;
		vocab?: string;

		// Microdata Attributes
		itemid?: string;
		itemID?: string;
		itemprop?: string;
		itemProp?: string;
		itemref?: string;
		itemRef?: string;
		itemscope?: boolean;
		itemScope?: boolean;
		itemtype?: string;
		itemType?: string;
	}

	type HTMLAttributeReferrerPolicy =
		| ''
		| 'no-referrer'
		| 'no-referrer-when-downgrade'
		| 'origin'
		| 'origin-when-cross-origin'
		| 'same-origin'
		| 'strict-origin'
		| 'strict-origin-when-cross-origin'
		| 'unsafe-url';

	type HTMLAttributeAnchorTarget = '_self' | '_blank' | '_parent' | '_top' | string;
	type HTMLAttributeCrossOrigin = 'anonymous' | 'use-credentials';

	// ============================================
	// Element-Specific HTML Attributes
	// ============================================

	interface AnchorHTMLAttributes<T extends EventTarget = HTMLAnchorElement>
		extends HTMLAttributes<T> {
		download?: any;
		href?: string;
		hreflang?: string;
		hrefLang?: string;
		media?: string;
		ping?: string;
		rel?: string;
		target?: HTMLAttributeAnchorTarget;
		type?: string;
		referrerpolicy?: HTMLAttributeReferrerPolicy;
		referrerPolicy?: HTMLAttributeReferrerPolicy;
	}

	interface AreaHTMLAttributes<T extends EventTarget = HTMLAreaElement>
		extends HTMLAttributes<T> {
		alt?: string;
		coords?: string;
		download?: any;
		href?: string;
		hreflang?: string;
		hrefLang?: string;
		media?: string;
		referrerpolicy?: HTMLAttributeReferrerPolicy;
		referrerPolicy?: HTMLAttributeReferrerPolicy;
		rel?: string;
		shape?: string;
		target?: HTMLAttributeAnchorTarget;
	}

	interface AudioHTMLAttributes<T extends EventTarget = HTMLAudioElement>
		extends MediaHTMLAttributes<T> {}

	interface BaseHTMLAttributes<T extends EventTarget = HTMLBaseElement>
		extends HTMLAttributes<T> {
		href?: string;
		target?: HTMLAttributeAnchorTarget;
	}

	interface BlockquoteHTMLAttributes<T extends EventTarget = HTMLQuoteElement>
		extends HTMLAttributes<T> {
		cite?: string;
	}

	interface ButtonHTMLAttributes<T extends EventTarget = HTMLButtonElement>
		extends HTMLAttributes<T> {
		command?: string;
		commandfor?: string;
		commandFor?: string;
		disabled?: boolean;
		form?: string;
		formaction?: string;
		formAction?: string;
		formenctype?: string;
		formEncType?: string;
		formmethod?: string;
		formMethod?: string;
		formnovalidate?: boolean;
		formNoValidate?: boolean;
		formtarget?: string;
		formTarget?: string;
		name?: string;
		popovertarget?: string;
		popoverTarget?: string;
		popovertargetaction?: 'hide' | 'show' | 'toggle';
		popoverTargetAction?: 'hide' | 'show' | 'toggle';
		type?: 'submit' | 'reset' | 'button';
		value?: string | number;
	}

	interface CanvasHTMLAttributes<T extends EventTarget = HTMLCanvasElement>
		extends HTMLAttributes<T> {
		height?: number | string;
		width?: number | string;
	}

	interface ColHTMLAttributes<T extends EventTarget = HTMLTableColElement>
		extends HTMLAttributes<T> {
		span?: number;
		width?: number | string;
	}

	interface ColgroupHTMLAttributes<T extends EventTarget = HTMLTableColElement>
		extends HTMLAttributes<T> {
		span?: number;
	}

	interface DataHTMLAttributes<T extends EventTarget = HTMLDataElement>
		extends HTMLAttributes<T> {
		value?: string | number;
	}

	interface DelHTMLAttributes<T extends EventTarget = HTMLModElement>
		extends HTMLAttributes<T> {
		cite?: string;
		datetime?: string;
		dateTime?: string;
	}

	interface DetailsHTMLAttributes<T extends EventTarget = HTMLDetailsElement>
		extends HTMLAttributes<T> {
		name?: string;
		open?: boolean;
	}

	interface DialogHTMLAttributes<T extends EventTarget = HTMLDialogElement>
		extends HTMLAttributes<T> {
		onCancel?: GenericEventHandler<T>;
		onClose?: GenericEventHandler<T>;
		open?: boolean;
		closedby?: 'none' | 'closerequest' | 'any';
		closedBy?: 'none' | 'closerequest' | 'any';
	}

	interface EmbedHTMLAttributes<T extends EventTarget = HTMLEmbedElement>
		extends HTMLAttributes<T> {
		height?: number | string;
		src?: string;
		type?: string;
		width?: number | string;
	}

	interface FieldsetHTMLAttributes<T extends EventTarget = HTMLFieldSetElement>
		extends HTMLAttributes<T> {
		disabled?: boolean;
		form?: string;
		name?: string;
	}

	interface FormHTMLAttributes<T extends EventTarget = HTMLFormElement>
		extends HTMLAttributes<T> {
		'accept-charset'?: string;
		acceptCharset?: string;
		action?: string;
		autocomplete?: string;
		autoComplete?: string;
		enctype?: string;
		encType?: string;
		method?: string;
		name?: string;
		novalidate?: boolean;
		noValidate?: boolean;
		rel?: string;
		target?: string;
	}

	interface IframeHTMLAttributes<T extends EventTarget = HTMLIFrameElement>
		extends HTMLAttributes<T> {
		allow?: string;
		allowFullScreen?: boolean;
		allowTransparency?: boolean;
		frameborder?: number | string;
		frameBorder?: number | string;
		height?: number | string;
		loading?: 'eager' | 'lazy';
		marginHeight?: number;
		marginWidth?: number;
		name?: string;
		referrerpolicy?: HTMLAttributeReferrerPolicy;
		referrerPolicy?: HTMLAttributeReferrerPolicy;
		sandbox?: string;
		scrolling?: string;
		seamless?: boolean;
		src?: string;
		srcdoc?: string;
		srcDoc?: string;
		width?: number | string;
	}

	interface ImgHTMLAttributes<T extends EventTarget = HTMLImageElement>
		extends HTMLAttributes<T> {
		alt?: string;
		crossorigin?: HTMLAttributeCrossOrigin;
		crossOrigin?: HTMLAttributeCrossOrigin;
		decoding?: 'async' | 'auto' | 'sync';
		fetchpriority?: 'high' | 'auto' | 'low';
		fetchPriority?: 'high' | 'auto' | 'low';
		height?: number | string;
		loading?: 'eager' | 'lazy';
		referrerpolicy?: HTMLAttributeReferrerPolicy;
		referrerPolicy?: HTMLAttributeReferrerPolicy;
		sizes?: string;
		src?: string;
		srcset?: string;
		srcSet?: string;
		usemap?: string;
		useMap?: string;
		width?: number | string;
	}

	type HTMLInputTypeAttribute =
		| 'button'
		| 'checkbox'
		| 'color'
		| 'date'
		| 'datetime-local'
		| 'email'
		| 'file'
		| 'hidden'
		| 'image'
		| 'month'
		| 'number'
		| 'password'
		| 'radio'
		| 'range'
		| 'reset'
		| 'search'
		| 'submit'
		| 'tel'
		| 'text'
		| 'time'
		| 'url'
		| 'week'
		| string;

	interface InputHTMLAttributes<T extends EventTarget = HTMLInputElement>
		extends HTMLAttributes<T> {
		accept?: string;
		alt?: string;
		autocomplete?: string;
		autoComplete?: string;
		capture?: 'user' | 'environment';
		checked?: boolean;
		defaultChecked?: boolean;
		defaultValue?: string | number;
		disabled?: boolean;
		enterKeyHint?: 'enter' | 'done' | 'go' | 'next' | 'previous' | 'search' | 'send';
		form?: string;
		formaction?: string;
		formAction?: string;
		formenctype?: string;
		formEncType?: string;
		formmethod?: string;
		formMethod?: string;
		formnovalidate?: boolean;
		formNoValidate?: boolean;
		formtarget?: string;
		formTarget?: string;
		height?: number | string;
		indeterminate?: boolean;
		list?: string;
		max?: number | string;
		maxlength?: number;
		maxLength?: number;
		min?: number | string;
		minlength?: number;
		minLength?: number;
		multiple?: boolean;
		name?: string;
		pattern?: string;
		placeholder?: string;
		readonly?: boolean;
		readOnly?: boolean;
		required?: boolean;
		size?: number;
		src?: string;
		step?: number | string;
		type?: HTMLInputTypeAttribute;
		value?: string | number;
		width?: number | string;
		onChange?: GenericEventHandler<T>;
	}

	interface InsHTMLAttributes<T extends EventTarget = HTMLModElement>
		extends HTMLAttributes<T> {
		cite?: string;
		datetime?: string;
		dateTime?: string;
	}

	interface KeygenHTMLAttributes<T extends EventTarget = HTMLUnknownElement>
		extends HTMLAttributes<T> {
		challenge?: string;
		disabled?: boolean;
		form?: string;
		keyType?: string;
		keyParams?: string;
		name?: string;
	}

	interface LabelHTMLAttributes<T extends EventTarget = HTMLLabelElement>
		extends HTMLAttributes<T> {
		for?: string;
		form?: string;
		htmlFor?: string;
	}

	interface LiHTMLAttributes<T extends EventTarget = HTMLLIElement>
		extends HTMLAttributes<T> {
		value?: string | number;
	}

	interface LinkHTMLAttributes<T extends EventTarget = HTMLLinkElement>
		extends HTMLAttributes<T> {
		as?: string;
		crossorigin?: HTMLAttributeCrossOrigin;
		crossOrigin?: HTMLAttributeCrossOrigin;
		fetchpriority?: 'high' | 'low' | 'auto';
		fetchPriority?: 'high' | 'low' | 'auto';
		href?: string;
		hreflang?: string;
		hrefLang?: string;
		integrity?: string;
		media?: string;
		imageSrcSet?: string;
		referrerpolicy?: HTMLAttributeReferrerPolicy;
		referrerPolicy?: HTMLAttributeReferrerPolicy;
		rel?: string;
		sizes?: string;
		type?: string;
		charset?: string;
		charSet?: string;
	}

	interface MapHTMLAttributes<T extends EventTarget = HTMLMapElement>
		extends HTMLAttributes<T> {
		name?: string;
	}

	interface MarqueeHTMLAttributes<T extends EventTarget = HTMLMarqueeElement>
		extends HTMLAttributes<T> {
		behavior?: 'scroll' | 'slide' | 'alternate';
		bgColor?: string;
		direction?: 'left' | 'right' | 'up' | 'down';
		height?: number | string;
		hspace?: number | string;
		loop?: number | string;
		scrollAmount?: number | string;
		scrollDelay?: number | string;
		trueSpeed?: boolean;
		vspace?: number | string;
		width?: number | string;
	}

	interface MediaHTMLAttributes<T extends EventTarget = HTMLMediaElement>
		extends HTMLAttributes<T> {
		autoplay?: boolean;
		autoPlay?: boolean;
		controls?: boolean;
		controlslist?: string;
		controlsList?: string;
		crossorigin?: HTMLAttributeCrossOrigin;
		crossOrigin?: HTMLAttributeCrossOrigin;
		currentTime?: number;
		defaultMuted?: boolean;
		defaultPlaybackRate?: number;
		disableremoteplayback?: boolean;
		disableRemotePlayback?: boolean;
		loop?: boolean;
		mediaGroup?: string;
		muted?: boolean;
		playbackRate?: number;
		preload?: 'auto' | 'metadata' | 'none';
		preservesPitch?: boolean;
		src?: string;
		srcObject?: MediaStream | MediaSource | Blob | File | null;
		volume?: string | number;
	}

	interface MenuHTMLAttributes<T extends EventTarget = HTMLMenuElement>
		extends HTMLAttributes<T> {
		type?: string;
	}

	interface MetaHTMLAttributes<T extends EventTarget = HTMLMetaElement>
		extends HTMLAttributes<T> {
		charset?: string;
		charSet?: string;
		content?: string;
		'http-equiv'?: string;
		httpEquiv?: string;
		name?: string;
		media?: string;
	}

	interface MeterHTMLAttributes<T extends EventTarget = HTMLMeterElement>
		extends HTMLAttributes<T> {
		form?: string;
		high?: number;
		low?: number;
		max?: number | string;
		min?: number | string;
		optimum?: number;
		value?: string | number;
	}

	interface ObjectHTMLAttributes<T extends EventTarget = HTMLObjectElement>
		extends HTMLAttributes<T> {
		classID?: string;
		data?: string;
		form?: string;
		height?: number | string;
		name?: string;
		type?: string;
		usemap?: string;
		useMap?: string;
		width?: number | string;
		wmode?: string;
	}

	interface OlHTMLAttributes<T extends EventTarget = HTMLOListElement>
		extends HTMLAttributes<T> {
		reversed?: boolean;
		start?: number;
		type?: '1' | 'a' | 'A' | 'i' | 'I';
	}

	interface OptgroupHTMLAttributes<T extends EventTarget = HTMLOptGroupElement>
		extends HTMLAttributes<T> {
		disabled?: boolean;
		label?: string;
	}

	interface OptionHTMLAttributes<T extends EventTarget = HTMLOptionElement>
		extends HTMLAttributes<T> {
		disabled?: boolean;
		label?: string;
		selected?: boolean;
		value?: string | number;
	}

	interface OutputHTMLAttributes<T extends EventTarget = HTMLOutputElement>
		extends HTMLAttributes<T> {
		for?: string;
		form?: string;
		htmlFor?: string;
		name?: string;
	}

	interface ParamHTMLAttributes<T extends EventTarget = HTMLParamElement>
		extends HTMLAttributes<T> {
		name?: string;
		value?: string | number;
	}

	interface ProgressHTMLAttributes<T extends EventTarget = HTMLProgressElement>
		extends HTMLAttributes<T> {
		max?: number | string;
		value?: string | number;
	}

	interface QuoteHTMLAttributes<T extends EventTarget = HTMLQuoteElement>
		extends HTMLAttributes<T> {
		cite?: string;
	}

	interface ScriptHTMLAttributes<T extends EventTarget = HTMLScriptElement>
		extends HTMLAttributes<T> {
		async?: boolean;
		charset?: string;
		charSet?: string;
		crossorigin?: HTMLAttributeCrossOrigin;
		crossOrigin?: HTMLAttributeCrossOrigin;
		defer?: boolean;
		integrity?: string;
		nomodule?: boolean;
		noModule?: boolean;
		referrerpolicy?: HTMLAttributeReferrerPolicy;
		referrerPolicy?: HTMLAttributeReferrerPolicy;
		src?: string;
		type?: string;
	}

	interface SelectHTMLAttributes<T extends EventTarget = HTMLSelectElement>
		extends HTMLAttributes<T> {
		autocomplete?: string;
		autoComplete?: string;
		defaultValue?: string | number;
		disabled?: boolean;
		form?: string;
		multiple?: boolean;
		name?: string;
		required?: boolean;
		size?: number;
		value?: string | number;
		onChange?: GenericEventHandler<T>;
	}

	interface SlotHTMLAttributes<T extends EventTarget = HTMLSlotElement>
		extends HTMLAttributes<T> {
		name?: string;
	}

	interface SourceHTMLAttributes<T extends EventTarget = HTMLSourceElement>
		extends HTMLAttributes<T> {
		height?: number | string;
		media?: string;
		sizes?: string;
		src?: string;
		srcset?: string;
		srcSet?: string;
		type?: string;
		width?: number | string;
	}

	interface StyleHTMLAttributes<T extends EventTarget = HTMLStyleElement>
		extends HTMLAttributes<T> {
		media?: string;
		scoped?: boolean;
		type?: string;
	}

	interface TableHTMLAttributes<T extends EventTarget = HTMLTableElement>
		extends HTMLAttributes<T> {
		cellPadding?: string;
		cellSpacing?: string;
		summary?: string;
		width?: number | string;
	}

	interface TdHTMLAttributes<T extends EventTarget = HTMLTableCellElement>
		extends HTMLAttributes<T> {
		align?: 'left' | 'center' | 'right' | 'justify' | 'char';
		colspan?: number;
		colSpan?: number;
		headers?: string;
		rowspan?: number;
		rowSpan?: number;
		scope?: string;
		abbr?: string;
		height?: number | string;
		width?: number | string;
		valign?: 'top' | 'middle' | 'bottom' | 'baseline';
	}

	interface TextareaHTMLAttributes<T extends EventTarget = HTMLTextAreaElement>
		extends HTMLAttributes<T> {
		autocomplete?: string;
		autoComplete?: string;
		cols?: number;
		defaultValue?: string | number;
		dirName?: string;
		disabled?: boolean;
		form?: string;
		maxlength?: number;
		maxLength?: number;
		minlength?: number;
		minLength?: number;
		name?: string;
		placeholder?: string;
		readOnly?: boolean;
		required?: boolean;
		rows?: number;
		value?: string | number;
		wrap?: string;
		onChange?: GenericEventHandler<T>;
	}

	interface ThHTMLAttributes<T extends EventTarget = HTMLTableCellElement>
		extends HTMLAttributes<T> {
		align?: 'left' | 'center' | 'right' | 'justify' | 'char';
		colspan?: number;
		colSpan?: number;
		headers?: string;
		rowspan?: number;
		rowSpan?: number;
		scope?: string;
		abbr?: string;
	}

	interface TimeHTMLAttributes<T extends EventTarget = HTMLTimeElement>
		extends HTMLAttributes<T> {
		datetime?: string;
		dateTime?: string;
	}

	interface TrackHTMLAttributes<T extends EventTarget = HTMLTrackElement>
		extends MediaHTMLAttributes<T> {
		default?: boolean;
		kind?: string;
		label?: string;
		srclang?: string;
		srcLang?: string;
	}

	interface VideoHTMLAttributes<T extends EventTarget = HTMLVideoElement>
		extends MediaHTMLAttributes<T> {
		disablePictureInPicture?: boolean;
		height?: number | string;
		playsinline?: boolean;
		playsInline?: boolean;
		poster?: string;
		width?: number | string;
	}

	// ============================================
	// SVG Attributes
	// ============================================

	interface SVGAttributes<Target extends EventTarget = SVGElement>
		extends HTMLAttributes<Target> {
		accentHeight?: number | string;
		accumulate?: 'none' | 'sum';
		additive?: 'replace' | 'sum';
		alignmentBaseline?:
			| 'auto'
			| 'baseline'
			| 'before-edge'
			| 'text-before-edge'
			| 'middle'
			| 'central'
			| 'after-edge'
			| 'text-after-edge'
			| 'ideographic'
			| 'alphabetic'
			| 'hanging'
			| 'mathematical'
			| 'inherit';
		'alignment-baseline'?:
			| 'auto'
			| 'baseline'
			| 'before-edge'
			| 'text-before-edge'
			| 'middle'
			| 'central'
			| 'after-edge'
			| 'text-after-edge'
			| 'ideographic'
			| 'alphabetic'
			| 'hanging'
			| 'mathematical'
			| 'inherit';
		allowReorder?: 'no' | 'yes';
		alphabetic?: number | string;
		amplitude?: number | string;
		arabicForm?: 'initial' | 'medial' | 'terminal' | 'isolated';
		ascent?: number | string;
		attributeName?: string;
		attributeType?: string;
		azimuth?: number | string;
		baseFrequency?: number | string;
		baselineShift?: number | string;
		'baseline-shift'?: number | string;
		baseProfile?: number | string;
		bbox?: number | string;
		begin?: number | string;
		bias?: number | string;
		by?: number | string;
		calcMode?: number | string;
		capHeight?: number | string;
		clip?: number | string;
		clipPath?: string;
		'clip-path'?: string;
		clipPathUnits?: number | string;
		clipRule?: number | string;
		'clip-rule'?: number | string;
		colorInterpolation?: number | string;
		'color-interpolation'?: number | string;
		colorInterpolationFilters?: 'auto' | 'sRGB' | 'linearRGB' | 'inherit';
		'color-interpolation-filters'?: 'auto' | 'sRGB' | 'linearRGB' | 'inherit';
		colorProfile?: number | string;
		colorRendering?: number | string;
		contentScriptType?: number | string;
		contentStyleType?: number | string;
		cursor?: number | string;
		cx?: number | string;
		cy?: number | string;
		d?: string;
		decelerate?: number | string;
		descent?: number | string;
		diffuseConstant?: number | string;
		direction?: number | string;
		display?: number | string;
		divisor?: number | string;
		dominantBaseline?: number | string;
		'dominant-baseline'?: number | string;
		dur?: number | string;
		dx?: number | string;
		dy?: number | string;
		edgeMode?: number | string;
		elevation?: number | string;
		enableBackground?: number | string;
		end?: number | string;
		exponent?: number | string;
		externalResourcesRequired?: number | string;
		fill?: string;
		fillOpacity?: number | string;
		'fill-opacity'?: number | string;
		fillRule?: 'nonzero' | 'evenodd' | 'inherit';
		'fill-rule'?: 'nonzero' | 'evenodd' | 'inherit';
		filter?: string;
		filterRes?: number | string;
		filterUnits?: number | string;
		floodColor?: number | string;
		'flood-color'?: number | string;
		floodOpacity?: number | string;
		'flood-opacity'?: number | string;
		focusable?: number | string;
		fontFamily?: string;
		'font-family'?: string;
		fontSize?: number | string;
		'font-size'?: number | string;
		fontSizeAdjust?: number | string;
		'font-size-adjust'?: number | string;
		fontStretch?: number | string;
		'font-stretch'?: number | string;
		fontStyle?: number | string;
		'font-style'?: number | string;
		fontVariant?: number | string;
		'font-variant'?: number | string;
		fontWeight?: number | string;
		'font-weight'?: number | string;
		format?: number | string;
		from?: number | string;
		fx?: number | string;
		fy?: number | string;
		g1?: number | string;
		g2?: number | string;
		glyphName?: number | string;
		glyphOrientationHorizontal?: number | string;
		glyphOrientationVertical?: number | string;
		glyphRef?: number | string;
		gradientTransform?: string;
		gradientUnits?: string;
		hanging?: number | string;
		height?: number | string;
		horizAdvX?: number | string;
		horizOriginX?: number | string;
		href?: string;
		hreflang?: string;
		hrefLang?: string;
		ideographic?: number | string;
		imageRendering?: number | string;
		'image-rendering'?: number | string;
		in2?: number | string;
		in?: string;
		intercept?: number | string;
		k1?: number | string;
		k2?: number | string;
		k3?: number | string;
		k4?: number | string;
		k?: number | string;
		kernelMatrix?: number | string;
		kernelUnitLength?: number | string;
		kerning?: number | string;
		keyPoints?: number | string;
		keySplines?: number | string;
		keyTimes?: number | string;
		lengthAdjust?: number | string;
		letterSpacing?: number | string;
		'letter-spacing'?: number | string;
		lightingColor?: number | string;
		'lighting-color'?: number | string;
		limitingConeAngle?: number | string;
		local?: number | string;
		markerEnd?: string;
		'marker-end'?: string;
		markerHeight?: number | string;
		markerMid?: string;
		'marker-mid'?: string;
		markerStart?: string;
		'marker-start'?: string;
		markerUnits?: number | string;
		markerWidth?: number | string;
		mask?: string;
		maskContentUnits?: number | string;
		maskUnits?: number | string;
		mathematical?: number | string;
		mode?: number | string;
		numOctaves?: number | string;
		offset?: number | string;
		opacity?: number | string;
		operator?: number | string;
		order?: number | string;
		orient?: number | string;
		orientation?: number | string;
		origin?: number | string;
		overflow?: number | string;
		overlinePosition?: number | string;
		overlineThickness?: number | string;
		paintOrder?: number | string;
		'paint-order'?: number | string;
		panose1?: number | string;
		pathLength?: number | string;
		patternContentUnits?: string;
		patternTransform?: number | string;
		patternUnits?: string;
		pointerEvents?: number | string;
		'pointer-events'?: number | string;
		points?: string;
		pointsAtX?: number | string;
		pointsAtY?: number | string;
		pointsAtZ?: number | string;
		preserveAlpha?: number | string;
		preserveAspectRatio?: string;
		primitiveUnits?: number | string;
		r?: number | string;
		radius?: number | string;
		refX?: number | string;
		refY?: number | string;
		renderingIntent?: number | string;
		repeatCount?: number | string;
		repeatDur?: number | string;
		requiredExtensions?: number | string;
		requiredFeatures?: number | string;
		restart?: number | string;
		result?: string;
		rotate?: number | string;
		rx?: number | string;
		ry?: number | string;
		scale?: number | string;
		seed?: number | string;
		shapeRendering?: number | string;
		'shape-rendering'?: number | string;
		slope?: number | string;
		spacing?: number | string;
		specularConstant?: number | string;
		specularExponent?: number | string;
		speed?: number | string;
		spreadMethod?: string;
		startOffset?: number | string;
		stdDeviation?: number | string;
		stemh?: number | string;
		stemv?: number | string;
		stitchTiles?: number | string;
		stopColor?: string;
		'stop-color'?: string;
		stopOpacity?: number | string;
		'stop-opacity'?: number | string;
		strikethroughPosition?: number | string;
		strikethroughThickness?: number | string;
		string?: number | string;
		stroke?: string;
		strokeDasharray?: string | number;
		'stroke-dasharray'?: string | number;
		strokeDashoffset?: string | number;
		'stroke-dashoffset'?: string | number;
		strokeLinecap?: 'butt' | 'round' | 'square' | 'inherit';
		'stroke-linecap'?: 'butt' | 'round' | 'square' | 'inherit';
		strokeLinejoin?: 'miter' | 'round' | 'bevel' | 'inherit';
		'stroke-linejoin'?: 'miter' | 'round' | 'bevel' | 'inherit';
		strokeMiterlimit?: string | number;
		'stroke-miterlimit'?: string | number;
		strokeOpacity?: number | string;
		'stroke-opacity'?: number | string;
		strokeWidth?: number | string;
		'stroke-width'?: number | string;
		surfaceScale?: number | string;
		systemLanguage?: number | string;
		tableValues?: number | string;
		targetX?: number | string;
		targetY?: number | string;
		textAnchor?: string;
		'text-anchor'?: string;
		textDecoration?: number | string;
		'text-decoration'?: number | string;
		textLength?: number | string;
		textRendering?: number | string;
		'text-rendering'?: number | string;
		to?: number | string;
		transform?: string;
		transformOrigin?: string;
		'transform-origin'?: string;
		type?: string;
		u1?: number | string;
		u2?: number | string;
		underlinePosition?: number | string;
		underlineThickness?: number | string;
		unicode?: number | string;
		unicodeBidi?: number | string;
		'unicode-bidi'?: number | string;
		unicodeRange?: number | string;
		unitsPerEm?: number | string;
		vAlphabetic?: number | string;
		values?: string;
		vectorEffect?: number | string;
		'vector-effect'?: number | string;
		version?: string;
		vertAdvY?: number | string;
		vertOriginX?: number | string;
		vertOriginY?: number | string;
		vHanging?: number | string;
		vIdeographic?: number | string;
		viewBox?: string;
		viewTarget?: number | string;
		visibility?: number | string;
		vMathematical?: number | string;
		width?: number | string;
		wordSpacing?: number | string;
		'word-spacing'?: number | string;
		writingMode?: number | string;
		'writing-mode'?: number | string;
		x1?: number | string;
		x2?: number | string;
		x?: number | string;
		xChannelSelector?: string;
		xHeight?: number | string;
		xlinkActuate?: string;
		'xlink:actuate'?: string;
		xlinkArcrole?: string;
		'xlink:arcrole'?: string;
		xlinkHref?: string;
		'xlink:href'?: string;
		xlinkRole?: string;
		'xlink:role'?: string;
		xlinkShow?: string;
		'xlink:show'?: string;
		xlinkTitle?: string;
		'xlink:title'?: string;
		xlinkType?: string;
		'xlink:type'?: string;
		xmlBase?: string;
		'xml:base'?: string;
		xmlLang?: string;
		'xml:lang'?: string;
		xmlns?: string;
		xmlnsXlink?: string;
		xmlSpace?: string;
		'xml:space'?: string;
		y1?: number | string;
		y2?: number | string;
		y?: number | string;
		yChannelSelector?: string;
		z?: number | string;
		zoomAndPan?: string;
	}

	interface PathAttributes {
		d: string;
	}

	// ============================================
	// MathML Attributes
	// ============================================

	interface MathMLAttributes<Target extends EventTarget = MathMLElement>
		extends HTMLAttributes<Target> {
		dir?: 'ltr' | 'rtl';
		displaystyle?: boolean;
		href?: string;
		mathbackground?: string;
		mathcolor?: string;
		mathsize?: string;
		nonce?: string;
		scriptlevel?: string;
	}

	interface AnnotationMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		encoding?: string;
		src?: string;
	}

	interface AnnotationXmlMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		encoding?: string;
		src?: string;
	}

	interface MActionMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		actiontype?: 'statusline' | 'toggle';
		selection?: string;
	}

	interface MathMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		display?: 'block' | 'inline';
	}

	interface MEncloseMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		notation?: string;
	}

	interface MErrorMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {}

	interface MFencedMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		close?: string;
		open?: string;
		separators?: string;
	}

	interface MFracMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		denomalign?: 'center' | 'left' | 'right';
		linethickness?: string;
		numalign?: 'center' | 'left' | 'right';
	}

	interface MiMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		mathvariant?:
			| 'normal'
			| 'bold'
			| 'italic'
			| 'bold-italic'
			| 'double-struck'
			| 'bold-fraktur'
			| 'script'
			| 'bold-script'
			| 'fraktur'
			| 'sans-serif'
			| 'bold-sans-serif'
			| 'sans-serif-italic'
			| 'sans-serif-bold-italic'
			| 'monospace'
			| 'initial'
			| 'tailed'
			| 'looped'
			| 'stretched';
	}

	interface MmultiScriptsMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		subscriptshift?: string;
		superscriptshift?: string;
	}

	interface MNMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {}

	interface MOMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		accent?: boolean;
		fence?: boolean;
		largeop?: boolean;
		lspace?: string;
		maxsize?: string;
		minsize?: string;
		movablelimits?: boolean;
		rspace?: string;
		separator?: boolean;
		stretchy?: boolean;
		symmetric?: boolean;
	}

	interface MOverMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		accent?: boolean;
	}

	interface MPaddedMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		depth?: string;
		height?: string;
		lspace?: string;
		voffset?: string;
		width?: string;
	}

	interface MPhantomMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {}

	interface MPrescriptsMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {}

	interface MRootMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {}

	interface MRowMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {}

	interface MSMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		lquote?: string;
		rquote?: string;
	}

	interface MSpaceMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		depth?: string;
		height?: string;
		width?: string;
	}

	interface MSqrtMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {}

	interface MStyleMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		background?: string;
		color?: string;
		fontsize?: string;
		fontstyle?: string;
		fontweight?: string;
		scriptminsize?: string;
		scriptsizemultiplier?: string;
	}

	interface MSubMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		subscriptshift?: string;
	}

	interface MSubsupMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		subscriptshift?: string;
		superscriptshift?: string;
	}

	interface MSupMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		superscriptshift?: string;
	}

	interface MTableMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		align?: 'axis' | 'baseline' | 'bottom' | 'center' | 'top';
		columnalign?: 'center' | 'left' | 'right';
		columnlines?: 'dashed' | 'none' | 'solid';
		columnspacing?: string;
		frame?: 'dashed' | 'none' | 'solid';
		framespacing?: string;
		rowalign?: 'axis' | 'baseline' | 'bottom' | 'center' | 'top';
		rowlines?: 'dashed' | 'none' | 'solid';
		rowspacing?: string;
		width?: string;
	}

	interface MTdMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		columnspan?: number;
		rowspan?: number;
		columnalign?: 'center' | 'left' | 'right';
		rowalign?: 'axis' | 'baseline' | 'bottom' | 'center' | 'top';
	}

	interface MTextMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {}

	interface MTrMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		columnalign?: 'center' | 'left' | 'right';
		rowalign?: 'axis' | 'baseline' | 'bottom' | 'center' | 'top';
	}

	interface MUnderMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		accentunder?: boolean;
	}

	interface MUnderoverMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {
		accent?: boolean;
		accentunder?: boolean;
	}

	interface SemanticsMathMLAttributes<T extends EventTarget>
		extends MathMLAttributes<T> {}

	// ============================================
	// Intrinsic Elements
	// ============================================

	interface IntrinsicSVGElements {
		svg: SVGAttributes<SVGSVGElement>;
		animate: SVGAttributes<SVGAnimateElement>;
		circle: SVGAttributes<SVGCircleElement>;
		animateMotion: SVGAttributes<SVGAnimateMotionElement>;
		animateTransform: SVGAttributes<SVGAnimateTransformElement>;
		clipPath: SVGAttributes<SVGClipPathElement>;
		defs: SVGAttributes<SVGDefsElement>;
		desc: SVGAttributes<SVGDescElement>;
		ellipse: SVGAttributes<SVGEllipseElement>;
		feBlend: SVGAttributes<SVGFEBlendElement>;
		feColorMatrix: SVGAttributes<SVGFEColorMatrixElement>;
		feComponentTransfer: SVGAttributes<SVGFEComponentTransferElement>;
		feComposite: SVGAttributes<SVGFECompositeElement>;
		feConvolveMatrix: SVGAttributes<SVGFEConvolveMatrixElement>;
		feDiffuseLighting: SVGAttributes<SVGFEDiffuseLightingElement>;
		feDisplacementMap: SVGAttributes<SVGFEDisplacementMapElement>;
		feDistantLight: SVGAttributes<SVGFEDistantLightElement>;
		feDropShadow: SVGAttributes<SVGFEDropShadowElement>;
		feFlood: SVGAttributes<SVGFEFloodElement>;
		feFuncA: SVGAttributes<SVGFEFuncAElement>;
		feFuncB: SVGAttributes<SVGFEFuncBElement>;
		feFuncG: SVGAttributes<SVGFEFuncGElement>;
		feFuncR: SVGAttributes<SVGFEFuncRElement>;
		feGaussianBlur: SVGAttributes<SVGFEGaussianBlurElement>;
		feImage: SVGAttributes<SVGFEImageElement>;
		feMerge: SVGAttributes<SVGFEMergeElement>;
		feMergeNode: SVGAttributes<SVGFEMergeNodeElement>;
		feMorphology: SVGAttributes<SVGFEMorphologyElement>;
		feOffset: SVGAttributes<SVGFEOffsetElement>;
		fePointLight: SVGAttributes<SVGFEPointLightElement>;
		feSpecularLighting: SVGAttributes<SVGFESpecularLightingElement>;
		feSpotLight: SVGAttributes<SVGFESpotLightElement>;
		feTile: SVGAttributes<SVGFETileElement>;
		feTurbulence: SVGAttributes<SVGFETurbulenceElement>;
		filter: SVGAttributes<SVGFilterElement>;
		foreignObject: SVGAttributes<SVGForeignObjectElement>;
		g: SVGAttributes<SVGGElement>;
		image: SVGAttributes<SVGImageElement>;
		line: SVGAttributes<SVGLineElement>;
		linearGradient: SVGAttributes<SVGLinearGradientElement>;
		marker: SVGAttributes<SVGMarkerElement>;
		mask: SVGAttributes<SVGMaskElement>;
		metadata: SVGAttributes<SVGMetadataElement>;
		mpath: SVGAttributes<SVGMPathElement>;
		path: SVGAttributes<SVGPathElement>;
		pattern: SVGAttributes<SVGPatternElement>;
		polygon: SVGAttributes<SVGPolygonElement>;
		polyline: SVGAttributes<SVGPolylineElement>;
		radialGradient: SVGAttributes<SVGRadialGradientElement>;
		rect: SVGAttributes<SVGRectElement>;
		set: SVGAttributes<SVGSetElement>;
		stop: SVGAttributes<SVGStopElement>;
		switch: SVGAttributes<SVGSwitchElement>;
		symbol: SVGAttributes<SVGSymbolElement>;
		text: SVGAttributes<SVGTextElement>;
		textPath: SVGAttributes<SVGTextPathElement>;
		tspan: SVGAttributes<SVGTSpanElement>;
		use: SVGAttributes<SVGUseElement>;
		view: SVGAttributes<SVGViewElement>;
	}

	interface IntrinsicMathMLElements {
		annotation: AnnotationMathMLAttributes<MathMLElement>;
		'annotation-xml': AnnotationXmlMathMLAttributes<MathMLElement>;
		maction: MActionMathMLAttributes<MathMLElement>;
		math: MathMathMLAttributes<MathMLElement>;
		menclose: MEncloseMathMLAttributes<MathMLElement>;
		merror: MErrorMathMLAttributes<MathMLElement>;
		mfenced: MFencedMathMLAttributes<MathMLElement>;
		mfrac: MFracMathMLAttributes<MathMLElement>;
		mi: MiMathMLAttributes<MathMLElement>;
		mmultiscripts: MmultiScriptsMathMLAttributes<MathMLElement>;
		mn: MNMathMLAttributes<MathMLElement>;
		mo: MOMathMLAttributes<MathMLElement>;
		mover: MOverMathMLAttributes<MathMLElement>;
		mpadded: MPaddedMathMLAttributes<MathMLElement>;
		mphantom: MPhantomMathMLAttributes<MathMLElement>;
		mprescripts: MPrescriptsMathMLAttributes<MathMLElement>;
		mroot: MRootMathMLAttributes<MathMLElement>;
		mrow: MRowMathMLAttributes<MathMLElement>;
		ms: MSMathMLAttributes<MathMLElement>;
		mspace: MSpaceMathMLAttributes<MathMLElement>;
		msqrt: MSqrtMathMLAttributes<MathMLElement>;
		mstyle: MStyleMathMLAttributes<MathMLElement>;
		msub: MSubMathMLAttributes<MathMLElement>;
		msubsup: MSubsupMathMLAttributes<MathMLElement>;
		msup: MSupMathMLAttributes<MathMLElement>;
		mtable: MTableMathMLAttributes<MathMLElement>;
		mtd: MTdMathMLAttributes<MathMLElement>;
		mtext: MTextMathMLAttributes<MathMLElement>;
		mtr: MTrMathMLAttributes<MathMLElement>;
		munder: MUnderMathMLAttributes<MathMLElement>;
		munderover: MUnderMathMLAttributes<MathMLElement>;
		semantics: SemanticsMathMLAttributes<MathMLElement>;
	}

	interface IntrinsicElements
		extends IntrinsicSVGElements,
			IntrinsicMathMLElements {
		a: AnchorHTMLAttributes<HTMLAnchorElement>;
		abbr: HTMLAttributes<HTMLElement>;
		address: HTMLAttributes<HTMLElement>;
		area: AreaHTMLAttributes<HTMLAreaElement>;
		article: HTMLAttributes<HTMLElement>;
		aside: HTMLAttributes<HTMLElement>;
		audio: AudioHTMLAttributes<HTMLAudioElement>;
		b: HTMLAttributes<HTMLElement>;
		base: BaseHTMLAttributes<HTMLBaseElement>;
		bdi: HTMLAttributes<HTMLElement>;
		bdo: HTMLAttributes<HTMLElement>;
		big: HTMLAttributes<HTMLElement>;
		blockquote: BlockquoteHTMLAttributes<HTMLQuoteElement>;
		body: HTMLAttributes<HTMLBodyElement>;
		br: HTMLAttributes<HTMLBRElement>;
		button: ButtonHTMLAttributes<HTMLButtonElement>;
		canvas: CanvasHTMLAttributes<HTMLCanvasElement>;
		caption: HTMLAttributes<HTMLTableCaptionElement>;
		cite: HTMLAttributes<HTMLElement>;
		code: HTMLAttributes<HTMLElement>;
		col: ColHTMLAttributes<HTMLTableColElement>;
		colgroup: ColgroupHTMLAttributes<HTMLTableColElement>;
		data: DataHTMLAttributes<HTMLDataElement>;
		datalist: HTMLAttributes<HTMLDataListElement>;
		dd: HTMLAttributes<HTMLElement>;
		del: DelHTMLAttributes<HTMLModElement>;
		details: DetailsHTMLAttributes<HTMLDetailsElement>;
		dfn: HTMLAttributes<HTMLElement>;
		dialog: DialogHTMLAttributes<HTMLDialogElement>;
		div: HTMLAttributes<HTMLDivElement>;
		dl: HTMLAttributes<HTMLDListElement>;
		dt: HTMLAttributes<HTMLElement>;
		em: HTMLAttributes<HTMLElement>;
		embed: EmbedHTMLAttributes<HTMLEmbedElement>;
		fieldset: FieldsetHTMLAttributes<HTMLFieldSetElement>;
		figcaption: HTMLAttributes<HTMLElement>;
		figure: HTMLAttributes<HTMLElement>;
		footer: HTMLAttributes<HTMLElement>;
		form: FormHTMLAttributes<HTMLFormElement>;
		h1: HTMLAttributes<HTMLHeadingElement>;
		h2: HTMLAttributes<HTMLHeadingElement>;
		h3: HTMLAttributes<HTMLHeadingElement>;
		h4: HTMLAttributes<HTMLHeadingElement>;
		h5: HTMLAttributes<HTMLHeadingElement>;
		h6: HTMLAttributes<HTMLHeadingElement>;
		head: HTMLAttributes<HTMLHeadElement>;
		header: HTMLAttributes<HTMLElement>;
		hgroup: HTMLAttributes<HTMLElement>;
		hr: HTMLAttributes<HTMLHRElement>;
		html: HTMLAttributes<HTMLHtmlElement>;
		i: HTMLAttributes<HTMLElement>;
		iframe: IframeHTMLAttributes<HTMLIFrameElement>;
		img: ImgHTMLAttributes<HTMLImageElement>;
		input: InputHTMLAttributes<HTMLInputElement>;
		ins: InsHTMLAttributes<HTMLModElement>;
		kbd: HTMLAttributes<HTMLElement>;
		keygen: KeygenHTMLAttributes<HTMLUnknownElement>;
		label: LabelHTMLAttributes<HTMLLabelElement>;
		legend: HTMLAttributes<HTMLLegendElement>;
		li: LiHTMLAttributes<HTMLLIElement>;
		link: LinkHTMLAttributes<HTMLLinkElement>;
		main: HTMLAttributes<HTMLElement>;
		map: MapHTMLAttributes<HTMLMapElement>;
		mark: HTMLAttributes<HTMLElement>;
		marquee: MarqueeHTMLAttributes<HTMLMarqueeElement>;
		menu: MenuHTMLAttributes<HTMLMenuElement>;
		menuitem: HTMLAttributes<HTMLUnknownElement>;
		meta: MetaHTMLAttributes<HTMLMetaElement>;
		meter: MeterHTMLAttributes<HTMLMeterElement>;
		nav: HTMLAttributes<HTMLElement>;
		noscript: HTMLAttributes<HTMLElement>;
		object: ObjectHTMLAttributes<HTMLObjectElement>;
		ol: OlHTMLAttributes<HTMLOListElement>;
		optgroup: OptgroupHTMLAttributes<HTMLOptGroupElement>;
		option: OptionHTMLAttributes<HTMLOptionElement>;
		output: OutputHTMLAttributes<HTMLOutputElement>;
		p: HTMLAttributes<HTMLParagraphElement>;
		param: ParamHTMLAttributes<HTMLParamElement>;
		picture: HTMLAttributes<HTMLPictureElement>;
		pre: HTMLAttributes<HTMLPreElement>;
		progress: ProgressHTMLAttributes<HTMLProgressElement>;
		q: QuoteHTMLAttributes<HTMLQuoteElement>;
		rp: HTMLAttributes<HTMLElement>;
		rt: HTMLAttributes<HTMLElement>;
		ruby: HTMLAttributes<HTMLElement>;
		s: HTMLAttributes<HTMLElement>;
		samp: HTMLAttributes<HTMLElement>;
		script: ScriptHTMLAttributes<HTMLScriptElement>;
		search: HTMLAttributes<HTMLElement>;
		section: HTMLAttributes<HTMLElement>;
		select: SelectHTMLAttributes<HTMLSelectElement>;
		slot: SlotHTMLAttributes<HTMLSlotElement>;
		small: HTMLAttributes<HTMLElement>;
		source: SourceHTMLAttributes<HTMLSourceElement>;
		span: HTMLAttributes<HTMLSpanElement>;
		strong: HTMLAttributes<HTMLElement>;
		style: StyleHTMLAttributes<HTMLStyleElement>;
		sub: HTMLAttributes<HTMLElement>;
		summary: HTMLAttributes<HTMLElement>;
		sup: HTMLAttributes<HTMLElement>;
		table: TableHTMLAttributes<HTMLTableElement>;
		tbody: HTMLAttributes<HTMLTableSectionElement>;
		td: TdHTMLAttributes<HTMLTableCellElement>;
		template: HTMLAttributes<HTMLTemplateElement>;
		textarea: TextareaHTMLAttributes<HTMLTextAreaElement>;
		tfoot: HTMLAttributes<HTMLTableSectionElement>;
		th: ThHTMLAttributes<HTMLTableCellElement>;
		thead: HTMLAttributes<HTMLTableSectionElement>;
		time: TimeHTMLAttributes<HTMLTimeElement>;
		title: HTMLAttributes<HTMLTitleElement>;
		tr: HTMLAttributes<HTMLTableRowElement>;
		track: TrackHTMLAttributes<HTMLTrackElement>;
		u: HTMLAttributes<HTMLElement>;
		ul: HTMLAttributes<HTMLUListElement>;
		var: HTMLAttributes<HTMLElement>;
		video: VideoHTMLAttributes<HTMLVideoElement>;
		wbr: HTMLAttributes<HTMLElement>;
	}
}
