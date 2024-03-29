//
// (C) Jan de Vaan 2007-2010, all rights reserved. See the accompanying "License.txt" for licensed use.
//

#ifndef CHARLS_SCAN
#define CHARLS_SCAN

#include "lookuptable.h"
#include "contextrunmode.h"
#include "context.h"
#include "colortransform.h"
#include "processline.h"
#include <sstream>

// This file contains the code for handling a "scan". Usually an image is encoded as a single scan.


#ifdef _MSC_VER
#pragma warning (disable: 4127) // conditional expression is constant (caused by some template methods that are not fully specialized) [VS2017]
#endif


extern CTable decodingTables[16];
extern std::vector<signed char> rgquant8Ll;
extern std::vector<signed char> rgquant10Ll;
extern std::vector<signed char> rgquant12Ll;
extern std::vector<signed char> rgquant16Ll;

inline int32_t ApplySign(int32_t i, int32_t sign) noexcept
{
    return (sign ^ i) - sign;
}


// Two alternatives for GetPredictedValue() (second is slightly faster due to reduced branching)

#if 0

inline int32_t GetPredictedValue(int32_t Ra, int32_t Rb, int32_t Rc)
{
    if (Ra < Rb)
    {
        if (Rc < Ra)
            return Rb;

        if (Rc > Rb)
            return Ra;
    }
    else
    {
        if (Rc < Rb)
            return Ra;

        if (Rc > Ra)
            return Rb;
    }

    return Ra + Rb - Rc;
}

#else

inline int32_t GetPredictedValue(int32_t Ra, int32_t Rb, int32_t Rc) noexcept
{
    // sign trick reduces the number of if statements (branches)
    const int32_t sgn = BitWiseSign(Rb - Ra);

    // is Ra between Rc and Rb?
    if ((sgn ^ (Rc - Ra)) < 0)
    {
        return Rb;
    }
    if ((sgn ^ (Rb - Rc)) < 0)
    {
        return Ra;
    }

    // default case, valid if Rc element of [Ra,Rb]
    return Ra + Rb - Rc;
}

#endif

inline int32_t UnMapErrVal(int32_t mappedError) noexcept
{
    const int32_t sign = static_cast<int32_t>(mappedError << (int32_t_bit_count-1)) >> (int32_t_bit_count-1);
    return sign ^ (mappedError >> 1);
}

inline int32_t GetMappedErrVal(int32_t Errval) noexcept
{
    const int32_t mappedError = (Errval >> (int32_t_bit_count-2)) ^ (2 * Errval);
    return mappedError;
}

inline int32_t ComputeContextID(int32_t Q1, int32_t Q2, int32_t Q3) noexcept
{
    return (Q1 * 9 + Q2) * 9 + Q3;
}


template<typename Traits, typename Strategy>
class JlsCodec : public Strategy
{
public:
    using PIXEL = typename Traits::PIXEL;
    using SAMPLE = typename Traits::SAMPLE;

    WARNING_SUPPRESS(26495) // false warning that _contextRunmode is uninitialized
    JlsCodec(const Traits& inTraits, const JlsParameters& params) :
        Strategy(params),
        traits(inTraits),
        _rect(),
        _width(params.width),
        T1(0),
        T2(0),
        T3(0),
        _RUNindex(0),
        _previousLine(),
        _currentLine(),
        _pquant(nullptr)
    {
        if (Info().interleaveMode == InterleaveMode::None)
        {
            Info().components = 1;
        }
    }
    WARNING_UNSUPPRESS()

    void SetPresets(const JpegLSPresetCodingParameters& presets) override
    {
        const JpegLSPresetCodingParameters presetDefault = ComputeDefault(traits.MAXVAL, traits.NEAR);

        InitParams(presets.Threshold1 != 0 ? presets.Threshold1 : presetDefault.Threshold1,
                   presets.Threshold2 != 0 ? presets.Threshold2 : presetDefault.Threshold2,
                   presets.Threshold3 != 0 ? presets.Threshold3 : presetDefault.Threshold3,
                   presets.ResetValue != 0 ? presets.ResetValue : presetDefault.ResetValue);
    }

    std::unique_ptr<ProcessLine> CreateProcess(ByteStreamInfo info) override;

    bool IsInterleaved() noexcept
    {
        if (Info().interleaveMode == InterleaveMode::None)
            return false;

        if (Info().components == 1)
            return false;

        return true;
    }

    JlsParameters& Info() noexcept
    {
        return Strategy::_params;
    }

    signed char QuantizeGratientOrg(int32_t Di) const noexcept;

    FORCE_INLINE int32_t QuantizeGratient(int32_t Di) const noexcept
    {
        ASSERT(QuantizeGratientOrg(Di) == *(_pquant + Di));
        return *(_pquant + Di);
    }

    void InitQuantizationLUT();

    int32_t DecodeValue(int32_t k, int32_t limit, int32_t qbpp);
    FORCE_INLINE void EncodeMappedValue(int32_t k, int32_t mappedError, int32_t limit);

    void IncrementRunIndex() noexcept
    {
        _RUNindex = std::min(31, _RUNindex + 1);
    }

    void DecrementRunIndex() noexcept
    {
        _RUNindex = std::max(0, _RUNindex - 1);
    }

    int32_t DecodeRIError(CContextRunMode& ctx);
    Triplet<SAMPLE> DecodeRIPixel(Triplet<SAMPLE> Ra, Triplet<SAMPLE> Rb);
    SAMPLE DecodeRIPixel(int32_t Ra, int32_t Rb);
    int32_t DecodeRunPixels(PIXEL Ra, PIXEL* startPos, int32_t cpixelMac);
    int32_t DoRunMode(int32_t startIndex, DecoderStrategy*);

    void EncodeRIError(CContextRunMode& ctx, int32_t Errval);
    SAMPLE EncodeRIPixel(int32_t x, int32_t Ra, int32_t Rb);
    Triplet<SAMPLE> EncodeRIPixel(Triplet<SAMPLE> x, Triplet<SAMPLE> Ra, Triplet<SAMPLE> Rb);
    void EncodeRunPixels(int32_t runLength, bool endOfLine);
    int32_t DoRunMode(int32_t index, EncoderStrategy*);

    FORCE_INLINE SAMPLE DoRegular(int32_t Qs, int32_t, int32_t pred, DecoderStrategy*);
    FORCE_INLINE SAMPLE DoRegular(int32_t Qs, int32_t x, int32_t pred, EncoderStrategy*);

    void DoLine(SAMPLE* pdummy);
    void DoLine(Triplet<SAMPLE>* pdummy);
    void DoScan();

    void InitParams(int32_t t1, int32_t t2, int32_t t3, int32_t nReset);

#if defined(__clang__)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Winconsistent-missing-override"
#endif

    // Note: depending on the base class EncodeScan OR DecodeScan will be virtual and abstract, cannot use override in all cases.
    size_t EncodeScan(std::unique_ptr<ProcessLine> processLine, ByteStreamInfo& compressedData);
    void DecodeScan(std::unique_ptr<ProcessLine> processLine, const JlsRect& rect, ByteStreamInfo& compressedData);

#if defined(__clang__)
#pragma clang diagnostic pop
#endif

protected:
    // codec parameters
    Traits traits;
    JlsRect _rect;
    int _width;
    int32_t T1;
    int32_t T2;
    int32_t T3;

    // compression context
    JlsContext _contexts[365];
    CContextRunMode _contextRunmode[2];
    int32_t _RUNindex;
    PIXEL* _previousLine;
    PIXEL* _currentLine;

    // quantization lookup table
    signed char* _pquant;
    std::vector<signed char> _rgquant;
};


// Encode/decode a single sample. Performance wise the #1 important functions
template<typename Traits, typename Strategy>
typename Traits::SAMPLE JlsCodec<Traits,Strategy>::DoRegular(int32_t Qs, int32_t, int32_t pred, DecoderStrategy*)
{
    const int32_t sign = BitWiseSign(Qs);
    JlsContext& ctx = _contexts[ApplySign(Qs, sign)];
    const int32_t k = ctx.GetGolomb();
    const int32_t Px = traits.CorrectPrediction(pred + ApplySign(ctx.C, sign));

    int32_t ErrVal;
    const Code& code = decodingTables[k].Get(Strategy::PeekByte());
    if (code.GetLength() != 0)
    {
        Strategy::Skip(code.GetLength());
        ErrVal = code.GetValue();
        ASSERT(std::abs(ErrVal) < 65535);
    }
    else
    {
        ErrVal = UnMapErrVal(DecodeValue(k, traits.LIMIT, traits.qbpp));
        if (std::abs(ErrVal) > 65535)
            throw charls_error(charls::ApiResult::InvalidCompressedData);
    }
    if (k == 0)
    {
        ErrVal = ErrVal ^ ctx.GetErrorCorrection(traits.NEAR);
    }
    ctx.UpdateVariables(ErrVal, traits.NEAR, traits.RESET);
    ErrVal = ApplySign(ErrVal, sign);
    return traits.ComputeReconstructedSample(Px, ErrVal);
}


template<typename Traits, typename Strategy>
typename Traits::SAMPLE JlsCodec<Traits,Strategy>::DoRegular(int32_t Qs, int32_t x, int32_t pred, EncoderStrategy*)
{
    const int32_t sign = BitWiseSign(Qs);
    JlsContext& ctx = _contexts[ApplySign(Qs, sign)];
    const int32_t k = ctx.GetGolomb();
    const int32_t Px = traits.CorrectPrediction(pred + ApplySign(ctx.C, sign));
    const int32_t ErrVal = traits.ComputeErrVal(ApplySign(x - Px, sign));

    EncodeMappedValue(k, GetMappedErrVal(ctx.GetErrorCorrection(k | traits.NEAR) ^ ErrVal), traits.LIMIT);
    ctx.UpdateVariables(ErrVal, traits.NEAR, traits.RESET);
    ASSERT(traits.IsNear(traits.ComputeReconstructedSample(Px, ApplySign(ErrVal, sign)), x));
    return static_cast<SAMPLE>(traits.ComputeReconstructedSample(Px, ApplySign(ErrVal, sign)));
}


// Functions to build tables used to decode short Golomb codes.

inline std::pair<int32_t, int32_t> CreateEncodedValue(int32_t k, int32_t mappedError) noexcept
{
    const int32_t highbits = mappedError >> k;
    return std::make_pair(highbits + k + 1, (static_cast<int32_t>(1) << k) | (mappedError & ((static_cast<int32_t>(1) << k) - 1)));
}


inline CTable InitTable(int32_t k) noexcept
{
    CTable table;
    for (short nerr = 0; ; nerr++)
    {
        // Q is not used when k != 0
        const int32_t merrval = GetMappedErrVal(nerr);
        const std::pair<int32_t, int32_t> paircode = CreateEncodedValue(k, merrval);
        if (static_cast<size_t>(paircode.first) > CTable::byte_bit_count)
            break;

        const Code code(nerr, static_cast<short>(paircode.first));
        table.AddEntry(static_cast<uint8_t>(paircode.second), code);
    }

    for (short nerr = -1; ; nerr--)
    {
        // Q is not used when k != 0
        const int32_t merrval = GetMappedErrVal(nerr);
        const std::pair<int32_t, int32_t> paircode = CreateEncodedValue(k, merrval);
        if (static_cast<size_t>(paircode.first) > CTable::byte_bit_count)
            break;

        const Code code = Code(nerr, static_cast<short>(paircode.first));
        table.AddEntry(static_cast<uint8_t>(paircode.second), code);
    }

    return table;
}


// Encoding/decoding of Golomb codes

template<typename Traits, typename Strategy>
int32_t JlsCodec<Traits, Strategy>::DecodeValue(int32_t k, int32_t limit, int32_t qbpp)
{
    const int32_t highbits = Strategy::ReadHighbits();

    if (highbits >= limit - (qbpp + 1))
        return Strategy::ReadValue(qbpp) + 1;

    if (k == 0)
        return highbits;

    return (highbits << k) + Strategy::ReadValue(k);
}


template<typename Traits, typename Strategy>
FORCE_INLINE void JlsCodec<Traits, Strategy>::EncodeMappedValue(int32_t k, int32_t mappedError, int32_t limit)
{
    int32_t highbits = mappedError >> k;

    if (highbits < limit - traits.qbpp - 1)
    {
        if (highbits + 1 > 31)
        {
            Strategy::AppendToBitStream(0, highbits / 2);
            highbits = highbits - highbits / 2;
        }
        Strategy::AppendToBitStream(1, highbits + 1);
        Strategy::AppendToBitStream((mappedError & ((1 << k) - 1)), k);
        return;
    }

    if (limit - traits.qbpp > 31)
    {
        Strategy::AppendToBitStream(0, 31);
        Strategy::AppendToBitStream(1, limit - traits.qbpp - 31);
    }
    else
    {
        Strategy::AppendToBitStream(1, limit - traits.qbpp);
    }
    Strategy::AppendToBitStream((mappedError - 1) & ((1 << traits.qbpp) - 1), traits.qbpp);
}


// Disable the Microsoft Static Analyzer warning: Potential comparison of a constant with another constant. (false warning, triggered by template construction)
#ifdef _PREFAST_
#pragma warning(push)
#pragma warning(disable:6326)
#endif

// Sets up a lookup table to "Quantize" sample difference.

template<typename Traits, typename Strategy>
void JlsCodec<Traits, Strategy>::InitQuantizationLUT()
{
    // for lossless mode with default parameters, we have precomputed the look up table for bit counts 8, 10, 12 and 16.
    if (traits.NEAR == 0 && traits.MAXVAL == (1 << traits.bpp) - 1)
    {
        const JpegLSPresetCodingParameters presets = ComputeDefault(traits.MAXVAL, traits.NEAR);
        if (presets.Threshold1 == T1 && presets.Threshold2 == T2 && presets.Threshold3 == T3)
        {
            if (traits.bpp == 8)
            {
                _pquant = &rgquant8Ll[rgquant8Ll.size() / 2];
                return;
            }
            if (traits.bpp == 10)
            {
                _pquant = &rgquant10Ll[rgquant10Ll.size() / 2];
                return;
            }
            if (traits.bpp == 12)
            {
                _pquant = &rgquant12Ll[rgquant12Ll.size() / 2];
                return;
            }
            if (traits.bpp == 16)
            {
                _pquant = &rgquant16Ll[rgquant16Ll.size() / 2];
                return;
            }
        }
    }

    const int32_t RANGE = 1 << traits.bpp;

    _rgquant.resize(static_cast<size_t>(RANGE) * 2);

    _pquant = &_rgquant[RANGE];
    for (int32_t i = -RANGE; i < RANGE; ++i)
    {
        _pquant[i] = QuantizeGratientOrg(i);
    }
}

#ifdef _PREFAST_
#pragma warning(pop)
#endif

template<typename Traits, typename Strategy>
signed char JlsCodec<Traits,Strategy>::QuantizeGratientOrg(int32_t Di) const noexcept
{
    if (Di <= -T3) return  -4;
    if (Di <= -T2) return  -3;
    if (Di <= -T1) return  -2;
    if (Di < -traits.NEAR)  return  -1;
    if (Di <=  traits.NEAR) return   0;
    if (Di < T1)   return   1;
    if (Di < T2)   return   2;
    if (Di < T3)   return   3;

    return  4;
}


// RI = Run interruption: functions that handle the sample terminating a run.

template<typename Traits, typename Strategy>
int32_t JlsCodec<Traits,Strategy>::DecodeRIError(CContextRunMode& ctx)
{
    const int32_t k = ctx.GetGolomb();
    const int32_t EMErrval = DecodeValue(k, traits.LIMIT - J[_RUNindex]-1, traits.qbpp);
    const int32_t Errval = ctx.ComputeErrVal(EMErrval + ctx._nRItype, k);
    ctx.UpdateVariables(Errval, EMErrval);
    return Errval;
}


template<typename Traits, typename Strategy>
void JlsCodec<Traits,Strategy>::EncodeRIError(CContextRunMode& ctx, int32_t Errval)
{
    const int32_t k = ctx.GetGolomb();
    const bool map = ctx.ComputeMap(Errval, k);
    const int32_t EMErrval = 2 * std::abs(Errval) - ctx._nRItype - static_cast<int32_t>(map);

    ASSERT(Errval == ctx.ComputeErrVal(EMErrval + ctx._nRItype, k));
    EncodeMappedValue(k, EMErrval, traits.LIMIT-J[_RUNindex]-1);
    ctx.UpdateVariables(Errval, EMErrval);
}


template<typename Traits, typename Strategy>
Triplet<typename Traits::SAMPLE> JlsCodec<Traits,Strategy>::DecodeRIPixel(Triplet<SAMPLE> Ra, Triplet<SAMPLE> Rb)
{
    const int32_t Errval1 = DecodeRIError(_contextRunmode[0]);
    const int32_t Errval2 = DecodeRIError(_contextRunmode[0]);
    const int32_t Errval3 = DecodeRIError(_contextRunmode[0]);

    return Triplet<SAMPLE>(traits.ComputeReconstructedSample(Rb.v1, Errval1 * Sign(Rb.v1  - Ra.v1)),
                           traits.ComputeReconstructedSample(Rb.v2, Errval2 * Sign(Rb.v2  - Ra.v2)),
                           traits.ComputeReconstructedSample(Rb.v3, Errval3 * Sign(Rb.v3  - Ra.v3)));
}


template<typename Traits, typename Strategy>
Triplet<typename Traits::SAMPLE> JlsCodec<Traits,Strategy>::EncodeRIPixel(Triplet<SAMPLE> x, Triplet<SAMPLE> Ra, Triplet<SAMPLE> Rb)
{
    const int32_t errval1 = traits.ComputeErrVal(Sign(Rb.v1 - Ra.v1) * (x.v1 - Rb.v1));
    EncodeRIError(_contextRunmode[0], errval1);

    const int32_t errval2 = traits.ComputeErrVal(Sign(Rb.v2 - Ra.v2) * (x.v2 - Rb.v2));
    EncodeRIError(_contextRunmode[0], errval2);

    const int32_t errval3 = traits.ComputeErrVal(Sign(Rb.v3 - Ra.v3) * (x.v3 - Rb.v3));
    EncodeRIError(_contextRunmode[0], errval3);

    return Triplet<SAMPLE>(traits.ComputeReconstructedSample(Rb.v1, errval1 * Sign(Rb.v1  - Ra.v1)),
                           traits.ComputeReconstructedSample(Rb.v2, errval2 * Sign(Rb.v2  - Ra.v2)),
                           traits.ComputeReconstructedSample(Rb.v3, errval3 * Sign(Rb.v3  - Ra.v3)));
}


template<typename Traits, typename Strategy>
typename Traits::SAMPLE JlsCodec<Traits,Strategy>::DecodeRIPixel(int32_t Ra, int32_t Rb)
{
    if (std::abs(Ra - Rb) <= traits.NEAR)
    {
        const int32_t ErrVal = DecodeRIError(_contextRunmode[1]);
        return static_cast<SAMPLE>(traits.ComputeReconstructedSample(Ra, ErrVal));
    }

    const int32_t ErrVal = DecodeRIError(_contextRunmode[0]);
    return static_cast<SAMPLE>(traits.ComputeReconstructedSample(Rb, ErrVal * Sign(Rb - Ra)));
}


template<typename Traits, typename Strategy>
typename Traits::SAMPLE JlsCodec<Traits,Strategy>::EncodeRIPixel(int32_t x, int32_t Ra, int32_t Rb)
{
    if (std::abs(Ra - Rb) <= traits.NEAR)
    {
        const int32_t ErrVal = traits.ComputeErrVal(x - Ra);
        EncodeRIError(_contextRunmode[1], ErrVal);
        return static_cast<SAMPLE>(traits.ComputeReconstructedSample(Ra, ErrVal));
    }

    const int32_t ErrVal = traits.ComputeErrVal((x - Rb) * Sign(Rb - Ra));
    EncodeRIError(_contextRunmode[0], ErrVal);
    return static_cast<SAMPLE>(traits.ComputeReconstructedSample(Rb, ErrVal * Sign(Rb - Ra)));
}


// RunMode: Functions that handle run-length encoding

template<typename Traits, typename Strategy>
void JlsCodec<Traits, Strategy>::EncodeRunPixels(int32_t runLength, bool endOfLine)
{
    while (runLength >= static_cast<int32_t>(1 << J[_RUNindex]))
    {
        Strategy::AppendOnesToBitStream(1);
        runLength = runLength - static_cast<int32_t>(1 << J[_RUNindex]);
        IncrementRunIndex();
    }

    if (endOfLine)
    {
        if (runLength != 0)
        {
            Strategy::AppendOnesToBitStream(1);
        }
    }
    else
    {
        Strategy::AppendToBitStream(runLength, J[_RUNindex] + 1); // leading 0 + actual remaining length
    }
}


template<typename Traits, typename Strategy>
int32_t JlsCodec<Traits, Strategy>::DecodeRunPixels(PIXEL Ra, PIXEL* startPos, int32_t cpixelMac)
{
    int32_t index = 0;
    while (Strategy::ReadBit())
    {
        const int count = std::min(1 << J[_RUNindex], int(cpixelMac - index));
        index += count;
        ASSERT(index <= cpixelMac);

        if (count == (1 << J[_RUNindex]))
        {
            IncrementRunIndex();
        }

        if (index == cpixelMac)
            break;
    }

    if (index != cpixelMac)
    {
        // incomplete run.
        index += (J[_RUNindex] > 0) ? Strategy::ReadValue(J[_RUNindex]) : 0;
    }

    if (index > cpixelMac)
        throw charls_error(charls::ApiResult::InvalidCompressedData);

    for (int32_t i = 0; i < index; ++i)
    {
        startPos[i] = Ra;
    }

    return index;
}

template<typename Traits, typename Strategy>
int32_t JlsCodec<Traits, Strategy>::DoRunMode(int32_t index, EncoderStrategy*)
{
    const int32_t ctypeRem = _width - index;
    PIXEL* ptypeCurX = _currentLine + index;
    const PIXEL* ptypePrevX = _previousLine + index;

    const PIXEL Ra = ptypeCurX[-1];

    int32_t runLength = 0;

    while (traits.IsNear(ptypeCurX[runLength],Ra))
    {
        ptypeCurX[runLength] = Ra;
        runLength++;

        if (runLength == ctypeRem)
            break;
    }

    EncodeRunPixels(runLength, runLength == ctypeRem);

    if (runLength == ctypeRem)
        return runLength;

    ptypeCurX[runLength] = EncodeRIPixel(ptypeCurX[runLength], Ra, ptypePrevX[runLength]);
    DecrementRunIndex();
    return runLength + 1;
}


template<typename Traits, typename Strategy>
int32_t JlsCodec<Traits, Strategy>::DoRunMode(int32_t startIndex, DecoderStrategy*)
{
    const PIXEL Ra = _currentLine[startIndex-1];

    const int32_t runLength = DecodeRunPixels(Ra, _currentLine + startIndex, _width - startIndex);
    const int32_t endIndex = startIndex + runLength;

    if (endIndex == _width)
        return endIndex - startIndex;

    // run interruption
    const PIXEL Rb = _previousLine[endIndex];
    _currentLine[endIndex] = DecodeRIPixel(Ra, Rb);
    DecrementRunIndex();
    return endIndex - startIndex + 1;
}


/// <summary>Encodes/Decodes a scan line of samples</summary>
template<typename Traits, typename Strategy>
void JlsCodec<Traits, Strategy>::DoLine(SAMPLE*)
{
    int32_t index = 0;
    int32_t Rb = _previousLine[index-1];
    int32_t Rd = _previousLine[index];

    while (index < _width)
    {
        const int32_t Ra = _currentLine[index -1];
        const int32_t Rc = Rb;
        Rb = Rd;
        Rd = _previousLine[index + 1];

        const int32_t Qs = ComputeContextID(QuantizeGratient(Rd - Rb), QuantizeGratient(Rb - Rc), QuantizeGratient(Rc - Ra));

        if (Qs != 0)
        {
            _currentLine[index] = DoRegular(Qs, _currentLine[index], GetPredictedValue(Ra, Rb, Rc), static_cast<Strategy*>(nullptr));
            index++;
        }
        else
        {
            index += DoRunMode(index, static_cast<Strategy*>(nullptr));
            Rb = _previousLine[index - 1];
            Rd = _previousLine[index];
        }
    }
}


/// <summary>Encodes/Decodes a scan line of triplets in ILV_SAMPLE mode</summary>
template<typename Traits, typename Strategy>
void JlsCodec<Traits, Strategy>::DoLine(Triplet<SAMPLE>*)
{
    int32_t index = 0;
    while(index < _width)
    {
        const Triplet<SAMPLE> Ra = _currentLine[index - 1];
        const Triplet<SAMPLE> Rc = _previousLine[index - 1];
        const Triplet<SAMPLE> Rb = _previousLine[index];
        const Triplet<SAMPLE> Rd = _previousLine[index + 1];

        const int32_t Qs1 = ComputeContextID(QuantizeGratient(Rd.v1 - Rb.v1), QuantizeGratient(Rb.v1 - Rc.v1), QuantizeGratient(Rc.v1 - Ra.v1));
        const int32_t Qs2 = ComputeContextID(QuantizeGratient(Rd.v2 - Rb.v2), QuantizeGratient(Rb.v2 - Rc.v2), QuantizeGratient(Rc.v2 - Ra.v2));
        const int32_t Qs3 = ComputeContextID(QuantizeGratient(Rd.v3 - Rb.v3), QuantizeGratient(Rb.v3 - Rc.v3), QuantizeGratient(Rc.v3 - Ra.v3));

        if (Qs1 == 0 && Qs2 == 0 && Qs3 == 0)
        {
            index += DoRunMode(index, static_cast<Strategy*>(nullptr));
        }
        else
        {
            Triplet<SAMPLE> Rx;
            Rx.v1 = DoRegular(Qs1, _currentLine[index].v1, GetPredictedValue(Ra.v1, Rb.v1, Rc.v1), static_cast<Strategy*>(nullptr));
            Rx.v2 = DoRegular(Qs2, _currentLine[index].v2, GetPredictedValue(Ra.v2, Rb.v2, Rc.v2), static_cast<Strategy*>(nullptr));
            Rx.v3 = DoRegular(Qs3, _currentLine[index].v3, GetPredictedValue(Ra.v3, Rb.v3, Rc.v3), static_cast<Strategy*>(nullptr));
            _currentLine[index] = Rx;
            index++;
        }
    }
}


// DoScan: Encodes or decodes a scan.
// In ILV_SAMPLE mode, multiple components are handled in DoLine
// In ILV_LINE mode, a call do DoLine is made for every component
// In ILV_NONE mode, DoScan is called for each component

template<typename Traits, typename Strategy>
void JlsCodec<Traits, Strategy>::DoScan()
{
    const int32_t pixelstride = _width + 4;
    const int components = Info().interleaveMode == charls::InterleaveMode::Line ? Info().components : 1;

    std::vector<PIXEL> vectmp(static_cast<size_t>(2) * components * pixelstride);
    std::vector<int32_t> rgRUNindex(components);

    for (int32_t line = 0; line < Info().height; ++line)
    {
        _previousLine = &vectmp[1];
        _currentLine = &vectmp[1 + static_cast<size_t>(components) * pixelstride];
        if ((line & 1) == 1)
        {
            std::swap(_previousLine, _currentLine);
        }

        Strategy::OnLineBegin(_width, _currentLine, pixelstride);

        for (int component = 0; component < components; ++component)
        {
            _RUNindex = rgRUNindex[component];

            // initialize edge pixels used for prediction
            _previousLine[_width] = _previousLine[_width - 1];
            _currentLine[-1] = _previousLine[0];
            DoLine(static_cast<PIXEL*>(nullptr)); // dummy argument for overload resolution

            rgRUNindex[component] = _RUNindex;
            _previousLine += pixelstride;
            _currentLine += pixelstride;
        }

        if (_rect.Y <= line && line < _rect.Y + _rect.Height)
        {
            Strategy::OnLineEnd(_rect.Width, _currentLine + _rect.X - (static_cast<size_t>(components) * pixelstride), pixelstride);
        }
    }

    Strategy::EndScan();
}


// Factory function for ProcessLine objects to copy/transform un encoded pixels to/from our scan line buffers.
template<typename Traits, typename Strategy>
std::unique_ptr<ProcessLine> JlsCodec<Traits, Strategy>::CreateProcess(ByteStreamInfo info)
{
    if (!IsInterleaved())
    {
        return info.rawData ?
            std::unique_ptr<ProcessLine>(std::make_unique<PostProcesSingleComponent>(info.rawData, Info(), sizeof(typename Traits::PIXEL))) :
            std::unique_ptr<ProcessLine>(std::make_unique<PostProcesSingleStream>(info.rawStream, Info(), sizeof(typename Traits::PIXEL)));
    }

    if (Info().colorTransformation == ColorTransformation::None)
        return std::make_unique<ProcessTransformed<TransformNone<typename Traits::SAMPLE>>>(info, Info(), TransformNone<SAMPLE>());

    if (Info().bitsPerSample == sizeof(SAMPLE) * 8)
    {
        switch (Info().colorTransformation)
        {
            case ColorTransformation::HP1: return std::make_unique<ProcessTransformed<TransformHp1<SAMPLE>>>(info, Info(), TransformHp1<SAMPLE>());
            case ColorTransformation::HP2: return std::make_unique<ProcessTransformed<TransformHp2<SAMPLE>>>(info, Info(), TransformHp2<SAMPLE>());
            case ColorTransformation::HP3: return std::make_unique<ProcessTransformed<TransformHp3<SAMPLE>>>(info, Info(), TransformHp3<SAMPLE>());
            default:
                std::ostringstream message;
                message << "Color transformation " << Info().colorTransformation << " is not supported.";
                throw charls_error(ApiResult::UnsupportedColorTransform, message.str());
        }
    }

    if (Info().bitsPerSample > 8)
    {
        const int shift = 16 - Info().bitsPerSample;
        switch (Info().colorTransformation)
        {
            case ColorTransformation::HP1: return std::make_unique<ProcessTransformed<TransformShifted<TransformHp1<uint16_t>>>>(info, Info(), TransformShifted<TransformHp1<uint16_t>>(shift));
            case ColorTransformation::HP2: return std::make_unique<ProcessTransformed<TransformShifted<TransformHp2<uint16_t>>>>(info, Info(), TransformShifted<TransformHp2<uint16_t>>(shift));
            case ColorTransformation::HP3: return std::make_unique<ProcessTransformed<TransformShifted<TransformHp3<uint16_t>>>>(info, Info(), TransformShifted<TransformHp3<uint16_t>>(shift));
            default:
                std::ostringstream message;
                message << "Color transformation " << Info().colorTransformation << " is not supported.";
                throw charls_error(ApiResult::UnsupportedColorTransform, message.str());
        }
    }

    throw charls_error(ApiResult::UnsupportedBitDepthForTransform);
}


// Setup codec for encoding and calls DoScan
WARNING_SUPPRESS(26433)
template<typename Traits, typename Strategy>
size_t JlsCodec<Traits, Strategy>::EncodeScan(std::unique_ptr<ProcessLine> processLine, ByteStreamInfo& compressedData)
{
    Strategy::_processLine = std::move(processLine);

    Strategy::Init(compressedData);
    DoScan();

    return Strategy::GetLength();
}


// Setup codec for decoding and calls DoScan
template<typename Traits, typename Strategy>
void JlsCodec<Traits, Strategy>::DecodeScan(std::unique_ptr<ProcessLine> processLine, const JlsRect& rect, ByteStreamInfo& compressedData)
{
    Strategy::_processLine = std::move(processLine);

    const uint8_t* compressedBytes = compressedData.rawData;
    _rect = rect;

    Strategy::Init(compressedData);
    DoScan();
    SkipBytes(compressedData, Strategy::GetCurBytePos() - compressedBytes);
}
WARNING_UNSUPPRESS()

// Initialize the codec data structures. Depends on JPEG-LS parameters like Threshold1-Threshold3.
template<typename Traits, typename Strategy>
void JlsCodec<Traits, Strategy>::InitParams(int32_t t1, int32_t t2, int32_t t3, int32_t nReset)
{
    T1 = t1;
    T2 = t2;
    T3 = t3;

    InitQuantizationLUT();

    const int32_t A = std::max(2, (traits.RANGE + 32) / 64);
    for (unsigned int Q = 0; Q < sizeof(_contexts) / sizeof(_contexts[0]); ++Q)
    {
        _contexts[Q] = JlsContext(A);
    }

    _contextRunmode[0] = CContextRunMode(std::max(2, (traits.RANGE + 32) / 64), 0, nReset);
    _contextRunmode[1] = CContextRunMode(std::max(2, (traits.RANGE + 32) / 64), 1, nReset);
    _RUNindex = 0;
}

#endif
